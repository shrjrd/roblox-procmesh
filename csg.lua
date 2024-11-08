local vec3 = Vector3.new
local floor = math.floor
local insert = table.insert
local bor = bit32.bor -- you can use a lua 5.1 bitwise library as a replacement in pre 2019 roblox
local vec3zero = vec3()

local planeEpsilon = 0.0002 --1e-5 -- tolerance used by `splitPolygon()` to decide if a point is on the plane

local newVertex, newNode, newPolygon, newPlane, newPlaneFromPoints

-- utility functions for lists stored in Lua tables
local function lmap(t, f)
	local res = {};
	for i,v in ipairs(t) do res[i] = f(v); end
	return res;
end

local function lcopy(t)
	local t2 = {}
	for k,v in pairs(t) do t2[k] = v end
	return t2
end

local function lappend(t1, t2)
	local res = lcopy(t1)
	for i,v in ipairs(t2) do insert(res, v) end
	return res
end

local function lreverse(t)
	for i=1, floor(#t / 2) do
		t[i], t[#t - i + 1] = t[#t - i + 1], t[i]
	end
	return t
end

-- Constructive Solid Geometry (CSG) is a modeling technique that uses Boolean
-- operations like union and intersection to combine 3D solids. self.library
-- implements CSG operations on meshes elegantly and concisely using BSP trees,
-- and is meant to serve as an easily understandable implementation of the
-- algorithm. All edge cases involving overlapping coplanar polygons in both
-- solids are correctly handled.
-- 
-- Example usage:
-- 
--     local cube = CSG.cube()
--     local sphere = CSG.sphere({ radius: 1.3 })
--     local polygons = cube.subtract(sphere).toPolygons()
-- 
--
-- ## Implementation Details
-- 
-- All CSG operations are implemented in terms of two functions, `clipTo()` and
-- `invert()`, which remove parts of a BSP tree inside another BSP tree and swap
-- solid and empty space, respectively. To find the union of `a` and `b`, we
-- want to remove everything in `a` inside `b` and everything in `b` inside `a`,
-- then combine polygons from `a` and `b` into one solid:
-- 
--     a:clipTo(b)
--     b:clipTo(a)
--     a:build(b:allPolygons())
-- 
-- The only tricky part is handling overlapping coplanar polygons in both trees.
-- The code above keeps both copies, but we need to keep them in one tree and
-- remove them in the other tree. To remove them from `b` we can clip the
-- inverse of `b` against `a`. The code for union now looks like self.
-- 
--     a:clipTo(b)
--     b:clipTo(a)
--     b:invert()
--     b:clipTo(a)
--     b:invert()
--     a:build(b:allPolygons())
-- 
-- Subtraction and intersection naturally follow from set operations. If
-- union is `A | B`, subtraction is `A - B = ~(~A | B)` and intersection is
-- `A & B = ~(~A | ~B)` where `~` is the complement operator.
-- 
-- ## License
-- 
-- Copyright (c) 2011 Evan Wallace (http:--madebyevan.com/), under the MIT license.

local m = {}

m.__index = m
m.__add = function (lhs, rhs) return lhs:union(rhs) end
m.__sub = function (lhs, rhs) return lhs:subtract(rhs) end
m.__mul = function (lhs, rhs) return lhs:intersect(rhs) end

--- Create an empty CSG primitive.
-- Holds a representation of triangle mesh that supports CSG manipulations,
-- inside a binary space partition tree. Two CSG primitives can be combined
-- using the `union()`, `subtract()`, and `intersect()` methods.
function m.new()
	local self = setmetatable({}, m)
	self.polygons = {}
	return self
end


-- Construct a CSG primitive from a list of `CSG.Polygon` instances.
function m.fromPolygons(polygons) 
	local self = m.new()
	self.polygons = polygons
	return self
end


--- Create CSG representation from a solid shape.
-- Solid is a table that stores vertices in nested table under key `vlist`,
-- and indices to triangle vertices in a flat table under key `ilist`.
function m.fromSolid(solid, shared)
	local self = m.new()
	for i = 1, #solid.ilist - 2, 3 do
		local v1 = solid.vlist[solid.ilist[i + 0]]
		local v2 = solid.vlist[solid.ilist[i + 1]]
		local v3 = solid.vlist[solid.ilist[i + 2]]
		local triangle = {
			newVertex(vec3(unpack(v1)), vec3(select(4, unpack(v1)))),
			newVertex(vec3(unpack(v2)), vec3(select(4, unpack(v2)))),
			newVertex(vec3(unpack(v3)), vec3(select(4, unpack(v3))))}
		insert(self.polygons, newPolygon(triangle, shared))
	end
	return self
end


--- Create a copy of CSG object.
-- This is not needed for normal use as union/subtract/intersect operations
-- internally clone the input objects to keep them preserved.
function m:clone()
	local other = m.new()
	other.polygons = lmap(self.polygons, function(p) return p:clone() end)
	return other
end


function m:transform(m)
	local x,y,z
	local transformed = {}
	for i, polygon in ipairs(self.polygons) do
		for j, v in ipairs(polygon.vertices) do
			if not transformed[v] then
				v.pos.x, v.pos.y, v.pos.z = m:mul(v.pos.x, v.pos.y, v.pos.z)
				v.normal.x, v.normal.y, v.normal.z = m:mul(v.normal.x, v.normal.y, v.normal.z)
				transformed[v] = true
			end
		end
	end
	return self
end


function m:toPolygons()
	return self.polygons
end


--- Create a CSG that covers volumes of both supplied CSG objects.
--     +-------+             +-------+
--     |       |             |       |
--     |   A   |             |       |
--     |    +--+----+   =>   |       +----+
--     +----+--+    |        +----+       |
--          |   B   |             |       |
--          |       |             |       |
--          +-------+             +-------+
function m:union(csg) 
	local a = newNode(self:clone().polygons)
	local b = newNode(csg:clone().polygons)
	a:clipTo(b)
	b:clipTo(a)
	b:invert()
	b:clipTo(a)
	b:invert()
	a:build(b:allPolygons())
	return m.fromPolygons(a:allPolygons())
end


--- Create a CSG that subtracts volume of second CSG from the first CSG.
--     +-------+             +-------+
--     |       |             |       |
--     |   A   |             |       |
--     |    +--+----+   =>   |    +--+
--     +----+--+    |        +----+
--          |   B   |
--          |       |
--          +-------+
-- 
function m:subtract(csg)
	local a = newNode(self:clone().polygons)
	local b = newNode(csg:clone().polygons)
	a:invert()
	a:clipTo(b)
	b:clipTo(a)
	b:invert()
	b:clipTo(a)
	b:invert()
	a:build(b:allPolygons())
	a:invert()
	return m.fromPolygons(a:allPolygons())
end


--- Create a CSG that covers only volume that both first and second CSG cover.
--     +-------+
--     |       |
--     |   A   |
--     |    +--+----+   =>   +--+
--     +----+--+    |        +--+
--          |   B   |
--          |       |
--          +-------+
-- 
function m:intersect(csg)
	local a = newNode(self:clone().polygons)
	local b = newNode(csg:clone().polygons)
	a:invert()
	b:clipTo(a)
	b:invert()
	a:clipTo(b)
	b:clipTo(a)
	a:build(b:allPolygons())
	a:invert()
	return m.fromPolygons(a:allPolygons())
end


--- Return a CSG with solid and empty space inverted.
function m:inverse()
	local csg = self:clone()
	lmap(csg.polygons, function(p) p:flip() end)
	return csg
end


-- # class Vertex

-- Represents a vertex of a polygon. Use your own vertex class instead of this
-- one to provide additional features like texture coordinates and vertex
-- colors. Custom vertex classes need to provide a `pos` property and `clone()`,
-- `flip()`, and `interpolate()` methods that behave analogous to the ones
-- defined by `CSG.Vertex`. self.class provides `normal` so convenience
-- functions like `CSG.sphere()` can return a smooth vertex normal, but `normal`
-- is not used anywhere else.
m.Vertex = {}
m.Vertex.__index = m.Vertex

function m.Vertex.new(pos, normal)
	local self = setmetatable({}, m.Vertex)  
	self.pos = pos
	self.normal = normal or vec3()
	return self
end
newVertex = m.Vertex.new


function m.Vertex:clone()
	return newVertex(self.pos, self.normal)
end


-- Invert all orientation-specific data (e.g. vertex normal). Called when the
-- orientation of a polygon is flipped.
function m.Vertex:flip()
	self.normal = -self.normal
end


-- Create a vertex between self.vertex and `other` by linearly
-- interpolating all properties using a parameter of `t`. Subclasses should
-- override self.to interpolate additional properties.
function m.Vertex:interpolate(other, t)
	return newVertex(
		self.pos:Lerp(other.pos, t),
		self.normal:Lerp(other.normal, t)
	)
end


-- # class Plane

-- Represents a plane in 3D space.
m.Plane = {}
m.Plane.__index = m.Plane


function m.Plane.new(normal, w)
	local self = setmetatable({}, m.Plane)
	self.normal = normal
	self.w = w
	return self
end
newPlane = m.Plane.new


function m.Plane.fromPoints(a, b, c)
	local n = (b-a):Cross(c-a).unit -- vec3(b):sub(a):cross(vec3(c):sub(a)):normalize()
	return newPlane(n, n:Dot(a))
end
newPlaneFromPoints = m.Plane.fromPoints


function m.Plane:clone()
	return newPlane(self.normal, self.w)
end


function m.Plane:flip()
	self.normal = -self.normal
	self.w = -self.w
end

-- Split `polygon` by self.plane if needed, then put the polygon or polygon
-- fragments in the appropriate lists. Coplanar polygons go into either
-- `coplanarFront` or `coplanarBack` depending on their orientation with
-- respect to self.plane. Polygons in front or in back of self.plane go into
-- either `front` or `back`.
function m.Plane:splitPolygon(polygon, coplanarFront, coplanarBack, front, back)
	local selfNormal, selfW = self.normal, self.w
	local PolygonVertices = polygon.vertices
	local NumPolygonVertices = #PolygonVertices
	local polygonType, COPLANAR, FRONT, BACK, SPANNING = 0, 0, 1, 2, 3
	-- Classify each point as well as the entire polygon into one of the above four classes.
	local types, lookup = {}, {}
	for i = 1, NumPolygonVertices do
		local NormalDotPosition = selfNormal:Dot(PolygonVertices[i].pos)
		local t = NormalDotPosition - selfW
		local ptype = COPLANAR
		if (t <= -planeEpsilon) then
			ptype = BACK
		elseif (t >= planeEpsilon) then
			ptype = FRONT
		end
		if (ptype ~= 0) then
			polygonType = bor(polygonType, ptype)
		end
		insert(types, ptype)
		lookup[i] = NormalDotPosition --lookup table instead of doing dot products twice
	end

	-- Put the polygon in the correct list, splitting it when necessary.
	if(polygonType == COPLANAR) then
		if selfNormal:Dot(polygon.plane.normal) > 0 then
			insert(coplanarFront, polygon)
		else 
			insert(coplanarBack, polygon)
		end
	elseif(polygonType == FRONT) then
		insert(front, polygon)
	elseif(polygonType == BACK) then
		insert(back, polygon)
	elseif(polygonType == SPANNING) then
		local f, b = {}, {}
		--for i, vi in ipairs(PolygonVertices) do
		for i = 1, NumPolygonVertices do
			local vi = PolygonVertices[i]
			local j = 1 + (i % NumPolygonVertices)
			local ti, vj = types[i], PolygonVertices[j]
			if (ti ~= BACK) then 
				insert(f, vi) 
			end
			if (ti ~= FRONT) then
				if (ti ~= BACK) then
					insert(b, vi:clone())
				else
					insert(b, vi)
				end
			end
			if (bor(ti, types[j]) == SPANNING) then
				local v = vi:interpolate(vj, (selfW - lookup[i]) / selfNormal:Dot(vj.pos - vi.pos))
				insert(f, v)
				insert(b, v:clone())
			end
		end
		if (#f >= 3) then insert(front, newPolygon(f, polygon.shared)) end
		if (#b >= 3) then insert(back, newPolygon(b, polygon.shared)) end
	end

end


-- # class Polygon

-- Represents a convex polygon. The vertices used to initialize a polygon must
-- be coplanar and form a convex loop. They do not have to be `CSG.Vertex`
-- instances but they must behave similarly (duck typing can be used for
-- customization).
-- 
-- Each convex polygon has a `shared` property, which is shared between all
-- polygons that are clones of each other or were split from the same polygon.
-- self. can be used to define per-polygon properties (such as surface color).
m.Polygon = {}
m.Polygon.__index = m.Polygon
function m.Polygon.new(vertices, shared)
	local self = setmetatable({}, m.Polygon)
	self.vertices = vertices
	self.shared = shared
	local v1 = vertices[1]
	local n = v1.normal
	if n ~= vec3zero then
		self.plane = newPlane(n, n:Dot(v1.pos))
	else
		self.plane = newPlaneFromPoints(v1.pos, vertices[2].pos, vertices[3].pos)
	end
	return self
end
newPolygon = m.Polygon.new


function m.Polygon:clone()
	local vertices = lmap(self.vertices, function(v) return v:clone() end)
	return newPolygon(vertices, self.shared)
end


function m.Polygon:flip()
	lmap(lreverse(self.vertices), function(v) v:flip() end)
	self.plane:flip()
end


--/ class Node --------------------------------------------------------------------------
-- Holds a node in a BSP tree. A BSP tree is built from a collection of polygons
-- by picking a polygon to split along. That polygon (and all other coplanar
-- polygons) are added directly to that node and the other polygons are added to
-- the front and/or back subtrees. This is not a leafy BSP tree since there is
-- no distinction between internal and leaf nodes.

m.Node = {}
m.Node.__index = m.Node


function m.Node.new(polygons)
	local self = setmetatable({}, m.Node)
	self.plane = nil
	self.front = nil
	self.back = nil
	self.polygons = {}
	if (polygons) then self:build(polygons) end
	return self
end
newNode = m.Node.new


function m.Node:clone()
	local other = newNode()
	other.plane = self.plane and self.plane:clone()
	other.front = self.front and self.front:clone()
	other.back = self.back and self.back:clone()
	other.polygons = lmap(self.polygons, function(p) return p:clone() end)
	return other
end


-- Convert solid space to empty space and empty space to solid space.
function m.Node:invert()
	for i, p in ipairs(self.polygons) do
		p:flip()
	end
	self.plane:flip()
	if (self.front) then self.front:invert() end
	if (self.back) then self.back:invert() end
	self.front, self.back = self.back, self.front
end


-- Recursively remove all polygons in `polygons` that are inside self.BSP
-- tree.
function m.Node:clipPolygons(polygons)
	if (not self.plane) then return lcopy(polygons) end
	local front, back = {}, {}
	for i, p in ipairs(polygons) do
		self.plane:splitPolygon(p, front, back, front, back)
	end
	if (self.front) then front = self.front:clipPolygons(front) end
	if (self.back) then back = self.back:clipPolygons(back)
	else back = {} end
	return lappend(front, back)
end


-- Remove all polygons in self.BSP tree that are inside the other BSP tree
-- `bsp`.
function m.Node:clipTo(bsp)
	self.polygons = bsp:clipPolygons(self.polygons)
	if (self.front) then self.front:clipTo(bsp) end
	if (self.back) then self.back:clipTo(bsp) end
end


-- Return a list of all polygons in self.BSP tree.
function m.Node:allPolygons()
	local polygons = lcopy(self.polygons)
	if (self.front) then polygons = lappend(polygons, self.front:allPolygons()) end
	if (self.back) then polygons = lappend(polygons, self.back:allPolygons()) end
	return polygons
end


-- Build a BSP tree out of `polygons`. When called on an existing tree, the
-- polygons are filtered down to the bottom of the tree and become new
-- nodes there. Each set of polygons is partitioned using the first polygon
-- (no heuristic is used to pick a good split).
function m.Node:build(polygons, depth)
	if (#polygons == 0) then return end
	depth = depth or 1
	local selfPolygons = self.polygons
	if (not self.plane) then 
		self.plane = polygons[1].plane:clone()  -- no heuristic, just use whatever the first polygon is
	end
	local front, back = {}, {}
	for i, p in ipairs(polygons) do
		self.plane:splitPolygon(p, selfPolygons, selfPolygons, front, back)
	end
	if depth > 8000 then print"csg stack overflow" return end
	if (#front > 0) then
		if (not self.front) then self.front = newNode() end
		self.front:build(front, depth + 1)
	end
	if (#back > 0) then
		if (not self.back) then self.back = newNode() end
		self.back:build(back, depth + 1)
	end
end


-- roblox-procmesh utility
local remove = table.remove

local abs = math.abs
local sin = math.sin
local cos = math.cos
local pi = math.pi
local clamp = math.clamp
local sqrt = math.sqrt
local min = math.min

local cf = CFrame.new

local vtws = cf().vectorToWorldSpace

local v3x = vec3(1,0,0)
local v3y = vec3(0,1,0)
local v3z = vec3(0,0,1)
local v3nx = vec3(-1, 0, 0)
local v3ny = vec3( 0,-1, 0)
local v3nz = vec3( 0, 0,-1)

local v1 = vec3( 1,  1, -1) -- top front right	
local v2 = vec3( 1, -1, -1) -- bottom front right	
local v3 = vec3(-1, -1, -1) -- bottom front left	
local v4 = vec3(-1,  1, -1) -- top front left
local v5 = vec3( 1,  1,  1) -- top back right	
local v6 = vec3( 1, -1,  1) -- bottom back right	
local v7 = vec3(-1, -1,  1) -- bottom back left
local v8 = vec3(-1,  1,  1) -- top back left

local CornerWedgeSlope1CFrame = cf(0,0,0, 0,-1,0, 1,0,0, 0,0,0)
local CornerWedgeSlope2CFrame = cf(0,0,0, 0,0,0, 0,0,1, 0,1,0)
local cn1 = vec3(-0.7071067690849304, 0.7071067690849304, 0)
local cn2 = vec3(0, 0.7071067690849304, 0.7071067690849304)
local WedgeSlopeCFrame = cf(0,0,0, 0,0,0, 0,0,1, 0,-1,0)
local wn1 = vec3(0, 0.7071067690849304, -0.7071067690849304)

-- "Roblox cylinders also have the interesting quirk of not being regular. The sides get smaller as the angle approaches pi*(2n+1)/4 and get larger as the angle approaches pi*n/2."
function m.fromAxisAlignedCylinder(position, size, shared)

end

function m.fromOrientedCylinder(cframe, size, shared)
	size = size * .5
	local segments = 24
	local a, b = cframe*cf(v3nx*size), cframe*cf(v3x*size) -- bottom, top of cylinder
	local ap, bp = a.p, b.p
	local r = min(size.y, size.z) --local sy, sz = size.y, size.z
	local polygons = {}
	for i = 0, segments - 1 do
		local theta = (i*(2*pi)/segments)
		local costheta, sintheta = cos(theta), sin(theta)
		local v1 = (a*cf(0, r*costheta, r*sintheta)).p
		local v2 = (b*cf(0, r*costheta, r*sintheta)).p
		theta = ((i + 1)*(2*pi)/segments)
		costheta, sintheta = cos(theta), sin(theta)
		local v3 = (a*cf(0, r*costheta, r*sintheta)).p
		local v4 = (b*cf(0, r*costheta, r*sintheta)).p
		insert(polygons, newPolygon({newVertex(v4), newVertex(v2), newVertex(v1)}, shared))
		insert(polygons, newPolygon({newVertex(v3), newVertex(v4), newVertex(v1)}, shared))
		insert(polygons, newPolygon({newVertex(v2), newVertex(v4), newVertex(bp)}, shared))
		insert(polygons, newPolygon({newVertex(v3), newVertex(v1), newVertex(ap)}, shared))
	end
	return setmetatable({["polygons"] = polygons}, m)
end

function m.fromAxisAlignedSphereMesh(position, size, shared)

end

function m.fromOrientedSphereMesh(cframe, size, shared)

end

function m.fromSphere(position, radius, shared) --radius is math.min(PartSize.x, PartSize.y, PartSize.z)
	local subdivisions = 2
	local phi = (1 + sqrt(5)) / 2
	local vlist = {
		{ -1,  phi, 0 },
		{  1,  phi, 0 },
		{ -1, -phi, 0 },
		{  1, -phi, 0 },

		{ 0, -1,  phi },
		{ 0,  1,  phi },
		{ 0, -1, -phi },
		{ 0,  1, -phi },

		{  phi, 0, -1 },
		{  phi, 0,  1 },
		{ -phi, 0, -1 },
		{ -phi, 0,  1 }
	}
	local ilist = {
		1, 12, 6,  1, 6, 2,  1, 2, 8,  1, 8, 11,  1, 11, 12,
		2, 6, 10,  6, 12, 5,  12, 11, 3,  11, 8, 7,  8, 2, 9,
		4, 10, 5,  4, 5, 3,  4, 3, 7,  4, 7, 9,  4, 9, 10,
		5, 10, 6,  3, 5, 12,  7, 3, 11,  9, 7, 8,  10, 9, 2
	}

	-- Cache vertex splits to avoid duplicates
	local splits = {}
	-- Splits self.vlist i and j, creating a new vertex and returning the index
	local function split(i, j)
		local key = i < j and (i .. ',' .. j) or (j .. ',' .. i)

		if not splits[key] then
			local x, y, z = (vlist[i][1] + vlist[j][1]) / 2, (vlist[i][2] + vlist[j][2]) / 2, (vlist[i][3] + vlist[j][3]) / 2
			insert(vlist, { x, y, z })
			splits[key] = #vlist
		end

		return splits[key]
	end
	-- Subdivide
	for _ = 1, subdivisions do
		for i = #ilist, 1, -3 do
			local v1, v2, v3 = ilist[i - 2], ilist[i - 1], ilist[i - 0]
			local a, b, c = split(v1, v2), split(v2, v3), split(v3, v1)

			insert(ilist, v1)
			insert(ilist, a)
			insert(ilist, c)

			insert(ilist, v2)
			insert(ilist, b)
			insert(ilist, a)

			insert(ilist, v3)
			insert(ilist, c)
			insert(ilist, b)

			insert(ilist, a)
			insert(ilist, b)
			insert(ilist, c)

			remove(ilist, i - 0)
			remove(ilist, i - 1)
			remove(ilist, i - 2)
		end
	end
	-- Normalize
	for _, v in ipairs(vlist) do
		local x, y, z = unpack(v)
		local length = sqrt(x * x + y * y + z * z) * 2
		v[1], v[2], v[3] = x / length, y / length, z / length
	end

	local px, py, pz = position.x, position.y, position.z
	for _, v in ipairs(vlist) do
		v[1] = (v[1]*radius) + px
		v[2] = (v[2]*radius) + py
		v[3] = (v[3]*radius) + pz
	end

	local polygons = {}
	for i = 1, #ilist - 2, 3 do
		local v1, v2, v3 = vlist[ilist[i + 0]], vlist[ilist[i + 1]], vlist[ilist[i + 2]]
		local triangle = {
			newVertex(vec3(unpack(v1)), vec3(select(4, unpack(v1)))),
			newVertex(vec3(unpack(v2)), vec3(select(4, unpack(v2)))),
			newVertex(vec3(unpack(v3)), vec3(select(4, unpack(v3))))}
		insert(polygons, newPolygon(triangle, shared))
	end
	return setmetatable({["polygons"] = polygons}, m)
end

function m.fromAxisAlignedCornerWedge(position, size, shared)
	size = size * .5
	local c1 = (position + (size*v1))
	local c2, c3 = (position + (size*v2)), (position + (size*v3))
	local c6, c7 = (position + (size*v6)), (position + (size*v7))
	return setmetatable(
		{
			["polygons"] = {
				newPolygon({newVertex(c6,v3ny), newVertex(c7,v3ny), newVertex(c3,v3ny), newVertex(c2,v3ny)}, shared),
				newPolygon({newVertex(c2,v3nz), newVertex(c3,v3nz), newVertex(c1,v3nz)}, shared),
				newPolygon({newVertex(c3, cn1), newVertex(c7, cn1), newVertex(c1, cn1)}, shared),
				newPolygon({newVertex(c7, cn2), newVertex(c6, cn2), newVertex(c1, cn2)}, shared),
				newPolygon({newVertex(c6, v3x), newVertex(c2, v3x), newVertex(c1, v3x)}, shared),
			}
		}, 
		m
	)
end

function m.fromOrientedCornerWedge(cframe, size, shared) 
	local n3 = vtws(cframe, (CornerWedgeSlope1CFrame*(size*2)).unit)
	local n4 = vtws(cframe, (CornerWedgeSlope2CFrame*(size*2)).unit)
	local lookVector, upVector, rightVector = cframe.lookVector, cframe.upVector, cframe.rightVector
	local n1, n2, n5 = -upVector, lookVector, rightVector
	size = size * .5
	local c1 = (cframe*cf(size*v1)).p
	local c2, c3 = (cframe*cf(size*v2)).p, (cframe*cf(size*v3)).p
	local c6, c7 = (cframe*cf(size*v6)).p, (cframe*cf(size*v7)).p
	return setmetatable(
		{
			["polygons"] = {
				newPolygon({newVertex(c6,n1), newVertex(c7,n1), newVertex(c3,n1), newVertex(c2,n1)}, shared), --bottom
				newPolygon({newVertex(c2,n2), newVertex(c3,n2), newVertex(c1,n2)}, shared),
				newPolygon({newVertex(c3,n3), newVertex(c7,n3), newVertex(c1,n3)}, shared),
				newPolygon({newVertex(c7,n4), newVertex(c6,n4), newVertex(c1,n4)}, shared),
				newPolygon({newVertex(c6,n5), newVertex(c2,n5), newVertex(c1,n5)}, shared),
			}
		}, 
		m
	)
end

function m.fromAxisAlignedWedge(position, size, shared)
	size = size * .5
	local c2, c3 = position + (size*v2), position + (size*v3)
	local c5, c6 = position + (size*v5), position + (size*v6)
	local c7, c8 = position + (size*v7), position + (size*v8)
	return setmetatable(
		{
			["polygons"] = {
				newPolygon({newVertex(c2, wn1), newVertex(c3, wn1), newVertex(c8, wn1), newVertex(c5, wn1)}, shared),
				newPolygon({newVertex(c7, v3z), newVertex(c6, v3z), newVertex(c5, v3z), newVertex(c8, v3z)}, shared),
				newPolygon({newVertex(c6,v3ny), newVertex(c7,v3ny), newVertex(c3,v3ny), newVertex(c2,v3ny)}, shared),
				newPolygon({newVertex(c3,v3nx), newVertex(c7,v3nx), newVertex(c8,v3nx)}, shared),
				newPolygon({newVertex(c6, v3x), newVertex(c2, v3x), newVertex(c5, v3x)}, shared),
			}
		}, 
		m
	)
end

function m.fromOrientedWedge(cframe, size, shared) 
	local n1 = vtws(cframe, (WedgeSlopeCFrame*size).unit)
	local lookVector, upVector, rightVector = cframe.lookVector, cframe.upVector, cframe.rightVector
	local n2, n3, n4, n5 = -lookVector, -upVector, -rightVector, rightVector
	size = size * .5
	local c2, c3 = (cframe*cf(size*v2)).p, (cframe*cf(size*v3)).p
	local c5, c6 = (cframe*cf(size*v5)).p, (cframe*cf(size*v6)).p
	local c7, c8 = (cframe*cf(size*v7)).p, (cframe*cf(size*v8)).p
	return setmetatable(
		{
			["polygons"] = {
				newPolygon({newVertex(c2,n1), newVertex(c3,n1), newVertex(c8,n1), newVertex(c5,n1)}, shared),
				newPolygon({newVertex(c7,n2), newVertex(c6,n2), newVertex(c5,n2), newVertex(c8,n2)}, shared),
				newPolygon({newVertex(c6,n3), newVertex(c7,n3), newVertex(c3,n3), newVertex(c2,n3)}, shared),
				newPolygon({newVertex(c3,n4), newVertex(c7,n4), newVertex(c8,n4)}, shared),
				newPolygon({newVertex(c6,n5), newVertex(c2,n5), newVertex(c5,n5)}, shared),
			}
		}, 
		m
	)
end

function m.fromAxisAlignedBlock(position, size, shared)
	size = size * .5
	local c1, c2 = position + (size*v1), position + (size*v2)
	local c3, c4 = position + (size*v3), position + (size*v4)
	local c5, c6 = position + (size*v5), position + (size*v6)
	local c7, c8 = position + (size*v7), position + (size*v8)
	return setmetatable(
		{
			["polygons"] = {
				newPolygon({newVertex(c2,v3nz), newVertex(c3,v3nz), newVertex(c4,v3nz), newVertex(c1,v3nz)}, shared),
				newPolygon({newVertex(c3,v3nx), newVertex(c7,v3nx), newVertex(c8,v3nx), newVertex(c4,v3nx)}, shared),
				newPolygon({newVertex(c7, v3z), newVertex(c6, v3z), newVertex(c5, v3z), newVertex(c8, v3z)}, shared),
				newPolygon({newVertex(c6 ,v3x), newVertex(c2, v3x), newVertex(c1, v3x), newVertex(c5, v3x)}, shared),
				newPolygon({newVertex(c1, v3y), newVertex(c4, v3y), newVertex(c8, v3y), newVertex(c5, v3y)}, shared),
				newPolygon({newVertex(c6,v3ny), newVertex(c7,v3ny), newVertex(c3,v3ny), newVertex(c2,v3ny)}, shared),
			}
		}, 
		m
	)
end

function m.fromOrientedBlock(cframe, size, shared)
	size = size * .5
	local c1, c2 = (cframe*cf(size*v1)).p, (cframe*cf(size*v2)).p
	local c3, c4 = (cframe*cf(size*v3)).p, (cframe*cf(size*v4)).p
	local c5, c6 = (cframe*cf(size*v5)).p, (cframe*cf(size*v6)).p
	local c7, c8 = (cframe*cf(size*v7)).p, (cframe*cf(size*v8)).p
	local lookVector, upVector, rightVector = cframe.lookVector, cframe.upVector, cframe.rightVector
	local n1, n2, n3, n4, n5, n6 =  lookVector, -rightVector, -lookVector,  rightVector, upVector, -upVector
	return setmetatable(
		{
			["polygons"] = {
				newPolygon({newVertex(c2,n1), newVertex(c3,n1), newVertex(c4,n1), newVertex(c1,n1)}, shared),
				newPolygon({newVertex(c3,n2), newVertex(c7,n2), newVertex(c8,n2), newVertex(c4,n2)}, shared),
				newPolygon({newVertex(c7,n3), newVertex(c6,n3), newVertex(c5,n3), newVertex(c8,n3)}, shared),
				newPolygon({newVertex(c6,n4), newVertex(c2,n4), newVertex(c1,n4), newVertex(c5,n4)}, shared),
				newPolygon({newVertex(c1,n5), newVertex(c4,n5), newVertex(c8,n5), newVertex(c5,n5)}, shared),
				newPolygon({newVertex(c6,n6), newVertex(c7,n6), newVertex(c3,n6), newVertex(c2,n6)}, shared),
			}
		}, 
		m
	)
end

local epsilon = planeEpsilon
local function Vector3FuzzyEq(v1, v2)
	local v1x, v2x = v1.X, v2.X
	if v1x == v2x or abs(v1x - v2x) <= (abs(v1x) + 1) * epsilon then
		local v1y, v2y = v1.Y, v2.Y
		if v1y == v2y or abs(v1y - v2y) <= (abs(v1y) + 1) * epsilon then
			local v1z, v2z = v1.Z, v2.Z
			if v1z == v2z or abs(v1z - v2z) <= (abs(v1z) + 1) * epsilon then
				return true
			end
		end
	end
	return false
end

local function FloatFuzzyEq(f1, f2)
	return f1 == f2 or abs(f1 - f2) <= (abs(f1) + 1) * epsilon
end

local function isCFrameAxisAligned(cframe)
	return (cframe.rightVector == v3x and cframe.upVector == v3y and cframe.lookVector == v3nz)
end

function m.getCSGFromPart(Part, Properties)
	if Part:IsA("BasePart") then
		local Shape = Part.Shape
		if Shape == Enum.PartType.Ball then
			local PartSize = Part.Size
			return m.fromSphere(Part.Position, min(PartSize.x, PartSize.y, PartSize.z), Properties)
		end
		local PartCFrame = Part.CFrame
		if Shape == Enum.PartType.Block then
			if isCFrameAxisAligned(PartCFrame) then
				return m.fromAxisAlignedBlock(Part.Position, Part.Size, Properties)
			else	
				return m.fromOrientedBlock(PartCFrame, Part.Size, Properties)
			end
		elseif Shape == Enum.PartType.CornerWedge then
			if isCFrameAxisAligned(PartCFrame) then
				return m.fromAxisAlignedCornerWedge(Part.Position, Part.Size, Properties)
			else	
				return m.fromOrientedCornerWedge(PartCFrame, Part.Size, Properties)
			end
		elseif Shape == Enum.PartType.Cylinder then
			if isCFrameAxisAligned(PartCFrame) then
				return m.fromOrientedCylinder(PartCFrame, Part.Size, Properties) --m.fromAxisAlignedCylinder(Part.Position, Part.Size, Properties)
			else	
				return m.fromOrientedCylinder(PartCFrame, Part.Size, Properties)
			end
		elseif Shape == Enum.PartType.Wedge then
			if isCFrameAxisAligned(PartCFrame) then
				return m.fromAxisAlignedWedge(Part.Position, Part.Size, Properties)
			else	
				return m.fromOrientedWedge(PartCFrame, Part.Size, Properties)
			end
		end
	else

	end
end

function m:getPolygonsFacingDirection(direction)
	local other = self:clone()
	local polygons = other.polygons
	for i = #polygons, 1, -1 do
		local d = polygons[i].plane.normal:Dot(direction)
		if d <= 0 or FloatFuzzyEq(d, 0) then
			remove(polygons, i)
		end
	end
	return other
end

function m:getPolygonsFacingPosition(position)
	local other = self:clone()
	local polygons = other.polygons
	for i = #polygons, 1, -1 do
		local v = polygons[i]
		local d = v.plane.normal:Dot((position-v.vertices[1].pos))
		if d <= 0 or FloatFuzzyEq(d, 0) then
			remove(polygons, i)
		end
	end
	return other
end

function m:offsetFromDirection(vector)
	local other = self:clone()
	local polygons = other.polygons
	for i1 = 1, #polygons do
		local v1 = polygons[i1].vertices
		for i2 = 1, #v1 do
			local v2 = v1[i2]
			v2.pos = v2.pos + vector
		end
	end
	return other
end

function m:offsetFromPosition(position, maxdistance)
	local other = self:clone()
	local polygons = other.polygons
	for i1 = 1, #polygons do
		local v1 = polygons[i1].vertices
		for i2 = 1, #v1 do
			local v2 = v1[i2]
			local pos = v2.pos
			local dir = pos - position
			v2.pos = pos + (dir.unit*clamp(maxdistance - dir.magnitude, 0, maxdistance))
		end
	end
	return other
end

function m:offsetTowardsPosition(position, mindistance)
	local other = self:clone()
	local polygons = other.polygons
	for i1 = 1, #polygons do
		local v1 = polygons[i1].vertices
		for i2 = 1, #v1 do
			local v2 = v1[i2]
			local pos = v2.pos
			local dir = position-pos
			v2.pos = pos + (dir.unit*(dir.magnitude - mindistance))
		end
	end
	return other
end



function m.stitch(csg1, csg2, shared)
	local polygons1, polygons2, newcsg = csg1.polygons, csg2.polygons, nil
	for i = 1, #polygons1 do
		local polygon1, polygon2 = polygons1[i], polygons2[i]
		local vertices1, vertices2 = polygon1.vertices, polygon2.vertices
		local d = ((vertices1[1].pos - vertices2[1].pos).unit):Dot(polygon1.plane.normal)
		if FloatFuzzyEq(d, 0) then
		elseif d < 0 then
			local polygons = {polygon1:clone(), polygon2:clone()}
			polygons[1]:flip()
			for i1 = 1, #vertices1 do
				local i2 = (i1 == #vertices1 and 1) or i1 + 1
				local midA, midB = vertices2[i1].pos, vertices1[i2].pos
				insert(polygons, newPolygon({newVertex(midA), newVertex(vertices1[i1].pos), newVertex(midB)}, shared))
				insert(polygons, newPolygon({newVertex(midA), newVertex(midB), newVertex(vertices2[i2].pos)}, shared))
			end
			newcsg = (newcsg and newcsg:union(setmetatable({["polygons"] = polygons}, m))) or setmetatable({["polygons"] = polygons}, m)
		elseif d > 0 then
			local polygons = {polygon1:clone(), polygon2:clone()}
			polygons[2]:flip()
			for i1 = 1, #vertices1 do
				local i2 = (i1 == #vertices1 and 1) or i1 + 1
				local midA, midB = vertices2[i1].pos, vertices1[i2].pos
				insert(polygons, newPolygon({newVertex(midB), newVertex(vertices1[i1].pos), newVertex(midA)}, shared))
				insert(polygons, newPolygon({newVertex(vertices2[i2].pos), newVertex(midB), newVertex(midA)}, shared))
			end
			newcsg = (newcsg and newcsg:union(setmetatable({["polygons"] = polygons}, m))) or setmetatable({["polygons"] = polygons}, m)
		end
	end
	return newcsg
end

function m.extrudeToPoint(csg, point, shared)
	local polygons, newcsg = csg.polygons, nil
	for i = 1, #polygons do
		local polygon = polygons[i]
		local verts = polygon.vertices
		local d = polygon.plane.normal:Dot((point-verts[1].pos))
		if FloatFuzzyEq(d, 0) then
		elseif d < 0 then
			local polys = {polygon:clone()}
			for i1 = 1, #verts do
				insert(polys, newPolygon({newVertex(point), newVertex(verts[(i1 == #verts and 1) or i1 + 1].pos), newVertex(verts[i1].pos)}, shared))
			end
			newcsg = (newcsg and newcsg:union(setmetatable({["polygons"] = polys}, m))) or setmetatable({["polygons"] = polys}, m)
		elseif d > 0 then
			local polys = {polygon:clone()}
			polys[1]:flip()
			for i1 = 1, #verts do
				insert(polys, newPolygon({newVertex(verts[i1].post), newVertex(verts[(i1 == #verts and 1) or i1 + 1].pos), newVertex(point)}, shared))
			end
			newcsg = (newcsg and newcsg:union(setmetatable({["polygons"] = polys}, m))) or setmetatable({["polygons"] = polys}, m)
		end
	end
	return newcsg
end

-- offset and stitch in one loop
function m.extrudeTowardsPosition(csg, point, distance, RemoveExtrudedSurface)

end

function m.extrudeFromPosition(csg, point, distance, RemoveExtrudedSurface)

end

function m.extrudeFromDirection(csg, offset, RemoveExtrudedSurface)

end

local function merge(list)
	local polygons = {}
	local NumPolygons = 0
	for i1 = 1, #list do
		local v1 = list[i1].polygons
		for i2 = 1, #v1 do
			NumPolygons = NumPolygons + 1
			polygons[NumPolygons] = v1[i2]:clone()
		end
	end
	return setmetatable({["polygons"] = polygons}, m)
end

local PlaneSize = 1e+4 -- higher sizes will cause incorrect results
local SliceSize = vec3(PlaneSize, PlaneSize, PlaneSize) -- (used as half size)
function m:bisect(PlanePoint, PlaneNormal, shared)
	local pos = PlanePoint + (PlaneNormal*SliceSize)
	local cube = m.fromOrientedBlock(CFrame.lookAt(pos, pos + PlaneNormal), SliceSize, shared)
	return self:intersect(cube), self:subtract(cube)
end

local function findGroup(group, v)
	for i1 = 1, #group do
		local g = group[i1]
		for i2 = 1, #g do
			if g[i2] == v or Vector3FuzzyEq(g[i2], v) then
				return i1
			end
		end
	end
end

-- flood fill mesh's vertices as graph to find disconnected pieces
-- polygons in csg in groups > optimal with lots of peices, suboptimal with minimal peices
-- polygon indices in groups > optimal with minimal peices, suboptimal with lots of peices
-- cloned polygons in groups > slightly optimal to csg, slightly suboptimal to indicies
function m:scission()
	local Polygons = self.polygons
	local PolygonGroups, groups = {}, {}

	for PolygonIndex = 1, #Polygons do

		local Polygon = Polygons[PolygonIndex]
		local Vertices = Polygon.vertices
		local NumVertices = #Vertices
		local GroupIndex

		for VertexIndex = 1, NumVertices do

			local A, B = Vertices[VertexIndex].pos, Vertices[(VertexIndex % NumVertices) + 1].pos
			local groupAi, groupBi = findGroup(groups, A), findGroup(groups, B)
			local groupA, groupB = groups[groupAi], groups[groupBi]
			if groupA and not groupB then
				insert(groupA, B)

				GroupIndex = groupAi
			elseif not groupA and groupB then
				insert(groupB, A)

				GroupIndex = groupBi
			elseif groupA and groupB then
				if groupA ~= groupB then
					for i = 1, #groupB do 
						insert(groupA, groupB[i])
					end
					remove(groups, groupBi)

					local polygonsA, polygonsB = PolygonGroups[groupAi], PolygonGroups[groupBi]
					if polygonsA then
						polygonsA, polygonsB = polygonsA.polygons, polygonsB.polygons
						for i = 1, #polygonsB do 
							insert(polygonsA, polygonsB[i]) 
						end
					else
						PolygonGroups[groupAi] = polygonsB
					end
					remove(PolygonGroups, groupBi)
				end

				GroupIndex = groupAi
			else
				insert(groups, {A, B})

				GroupIndex = #groups
			end

		end

		local PolygonGroup = PolygonGroups[GroupIndex]
		if not PolygonGroup then
			PolygonGroups[GroupIndex] = setmetatable({["polygons"] = {Polygon:clone()}}, m)
		else
			insert(PolygonGroup.polygons, Polygon:clone())
		end

	end

	return PolygonGroups
end



return m
