local vec3 = Vector3.new
local floor = math.floor
local insert = table.insert


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
			m.Vertex.new(vec3(unpack(v1)), vec3(select(4, unpack(v1)))),
			m.Vertex.new(vec3(unpack(v2)), vec3(select(4, unpack(v2)))),
			m.Vertex.new(vec3(unpack(v3)), vec3(select(4, unpack(v3))))}
		insert(self.polygons, m.Polygon.new(triangle, shared))
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
	local a = m.Node.new(self:clone().polygons)
	local b = m.Node.new(csg:clone().polygons)
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
	local a = m.Node.new(self:clone().polygons)
	local b = m.Node.new(csg:clone().polygons)
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
	local a = m.Node.new(self:clone().polygons)
	local b = m.Node.new(csg:clone().polygons)
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
	self.pos = pos --vec3(pos)
	self.normal = vec3(normal)
	return self
end


function m.Vertex:clone()
	return m.Vertex.new(self.pos, self.normal)
end


-- Invert all orientation-specific data (e.g. vertex normal). Called when the
-- orientation of a polygon is flipped.
function m.Vertex:flip()
	self.normal = self.normal*-1 --:mul(-1)
end


-- Create a vertex between self.vertex and `other` by linearly
-- interpolating all properties using a parameter of `t`. Subclasses should
-- override self.to interpolate additional properties.
function m.Vertex:interpolate(other, t)
	return m.Vertex.new(
		self.pos:Lerp(other.pos, t),
		self.normal:Lerp(other.normal, t)
	)
end


-- # class Plane

-- Represents a plane in 3D space.
m.Plane = {
	EPSILON = 1e-3 --1e-5 -- tolerance used by `splitPolygon()` to decide if a point is on the plane
}
m.Plane.__index = m.Plane


function m.Plane.new(normal, w)
	local self = setmetatable({}, m.Plane)
	self.normal = normal
	self.w = w
	return self
end


function m.Plane.fromPoints(a, b, c)
	local n = (b-a):Cross(c-a).unit -- vec3(b):sub(a):cross(vec3(c):sub(a)):normalize()
	return m.Plane.new(n, n:Dot(a))
end


function m.Plane:clone()
	return m.Plane.new(self.normal, self.w)
end


function m.Plane:flip()
	self.normal = self.normal*-1 --:mul(-1)
	self.w = -self.w
end


-- Split `polygon` by self.plane if needed, then put the polygon or polygon
-- fragments in the appropriate lists. Coplanar polygons go into either
-- `coplanarFront` or `coplanarBack` depending on their orientation with
-- respect to self.plane. Polygons in front or in back of self.plane go into
-- either `front` or `back`.
function m.Plane:splitPolygon(polygon, coplanarFront, coplanarBack, front, back)
	local COPLANAR = 0
	local FRONT = 1
	local BACK = 2
	local SPANNING = 3

	-- Classify each point as well as the entire polygon into one of the above
	-- four classes.
	local polygonType = 0
	local types = {}
	for i, v in ipairs(polygon.vertices) do
		local t = self.normal:Dot(v.pos) - self.w
		local ptype = COPLANAR
		if (t <= -m.Plane.EPSILON) then
			ptype = BACK
		elseif (t >= m.Plane.EPSILON) then
			ptype = FRONT
		end
		if (ptype ~= 0) then
			polygonType = bit32.bor(polygonType, ptype)
		end
		insert(types, ptype)
	end

	-- Put the polygon in the correct list, splitting it when necessary.
	if(polygonType == COPLANAR) then
		if self.normal:Dot(polygon.plane.normal) > 0 then
			insert(coplanarFront, polygon)
		else 
			insert(coplanarBack, polygon)
		end
	elseif(polygonType == FRONT) then
		insert(front, polygon)
	elseif(polygonType == BACK) then
		insert(back, polygon)
	elseif(polygonType == SPANNING) then
		local f = {}
		local b = {}
		for i, vi in ipairs(polygon.vertices) do
			local j = 1 + (i % #polygon.vertices)
			local ti = types[i]
			local tj = types[j]
			local vj = polygon.vertices[j]
			if (ti ~= BACK) then insert(f, vi) end
			if (ti ~= FRONT) then
				if (ti ~= BACK) then
					insert(b, vi:clone())
				else
					insert(b, vi)
				end
			end
			if (bit32.bor(ti, tj) == SPANNING) then
				--local t = (self.w - self.normal:Dot(vi.pos)) / self.normal:Dot(vec3(vj.pos):sub(vi.pos))
				local t = (self.w - self.normal:Dot(vi.pos)) / self.normal:Dot(vj.pos - vi.pos)
				local v = vi:interpolate(vj, t)
				insert(f, v)
				insert(b, v:clone())
			end
		end
		if (#f >= 3) then insert(front, m.Polygon.new(f, polygon.shared)) end
		if (#b >= 3) then insert(back, m.Polygon.new(b, polygon.shared)) end
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
	self.plane = m.Plane.fromPoints(vertices[1].pos, vertices[2].pos, vertices[3].pos)
	return self
end


function m.Polygon:clone()
	local vertices = lmap(self.vertices, function(v) return v:clone() end)
	return m.Polygon.new(vertices, self.shared)
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


function m.Node:clone()
	local other = m.Node.new()
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
	local temp = self.front
	self.front = self.back
	self.back = temp
end


-- Recursively remove all polygons in `polygons` that are inside self.BSP
-- tree.
function m.Node:clipPolygons(polygons)
	if (not self.plane) then return lcopy(polygons) end
	local front = {}
	local back = {}
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
	depth = depth or 1
	if (#polygons == 0) then return end
	if (not self.plane) then 
		self.plane = polygons[1].plane:clone()  -- no heuristic, just use whatever the first polygon is
	end
	local front = {}
	local back = {}
	for i, p in ipairs(polygons) do
		self.plane:splitPolygon(p, self.polygons, self.polygons, front, back)
	end
	if depth > 500 then --[[print"stack overflow"]] return end --stack overflow
	if (#front > 0) then
		if (not self.front) then self.front = m.Node.new() end
		self.front:build(front, depth + 1)
	end
	if (#back > 0) then
		if (not self.back) then self.back = m.Node.new() end
		self.back:build(back, depth + 1)
	end
end

-- Extra Functions
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

local newVertex = m.Vertex.new
local newPolygon = m.Polygon.new

local remove = table.remove

local abs = math.abs
local sin = math.sin
local cos = math.cos
local pi = math.pi
local clamp = math.clamp

local cf = CFrame.new

local epsilon = 1e-3


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


-- "Roblox cylinders also have the interesting quirk of not being regular. The sides get smaller as the angle approaches pi*(2n+1)/4 and get larger as the angle approaches pi*n/2."
function m.fromAxisAlignedCylinder(position, size, shared)
	
end

function m.fromOrientedCylinder(cframe, size, shared)
	local segments = 24
	local a, b = cframe*cf(v3nx*size), cframe*cf(v3x*size) -- bottom, top of cylinder
	local ap, bp, sy, sz = a.p, b.p, size.y, size.z
	local polygons = {}
	for i = 0, segments - 1 do
		local theta = (i*(2*pi)/segments)
		local costheta, sintheta = cos(theta), sin(theta)
		local v1 = (a*cf(0, sy*costheta, sz*sintheta)).p
		local v2 = (b*cf(0, sy*costheta, sz*sintheta)).p
		theta = ((i + 1)*(2*pi)/segments)
		costheta, sintheta = cos(theta), sin(theta)
		local v3 = (a*cf(0, sy*costheta, sz*sintheta)).p
		local v4 = (b*cf(0, sy*costheta, sz*sintheta)).p
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

function m.fromSphere(position, radius, shared)
	
end

function m.fromAxisAlignedCornerWedge(position, size, shared)
	local c1 = (position + (size*v1)).p
	local c2, c3 = (position + (size*v2)).p, (position + (size*v3)).p
	local c6, c7 = (position + (size*v6)).p, (position + (size*v7)).p
	return setmetatable(
		{
			["polygons"] = {
				newPolygon({newVertex(c3), newVertex(c2), newVertex(c7)}, shared),
				newPolygon({newVertex(c7), newVertex(c2), newVertex(c6)}, shared),
				newPolygon({newVertex(c3), newVertex(c1), newVertex(c2)}, shared),
				newPolygon({newVertex(c6), newVertex(c2), newVertex(c1)}, shared),
				newPolygon({newVertex(c6), newVertex(c1), newVertex(c7)}, shared),
				newPolygon({newVertex(c3), newVertex(c7), newVertex(c1)}, shared),
			}
		}, 
		m
	)
end

function m.fromOrientedCornerWedge(cframe, size, shared) 
	local c1 = (cframe*cf(size*v1)).p
	local c2, c3 = (cframe*cf(size*v2)).p, (cframe*cf(size*v3)).p
	local c6, c7 = (cframe*cf(size*v6)).p, (cframe*cf(size*v7)).p
	return setmetatable(
		{
			["polygons"] = {
				newPolygon({newVertex(c3), newVertex(c2), newVertex(c7)}, shared),
				newPolygon({newVertex(c7), newVertex(c2), newVertex(c6)}, shared),
				newPolygon({newVertex(c3), newVertex(c1), newVertex(c2)}, shared),
				newPolygon({newVertex(c6), newVertex(c2), newVertex(c1)}, shared),
				newPolygon({newVertex(c6), newVertex(c1), newVertex(c7)}, shared),
				newPolygon({newVertex(c3), newVertex(c7), newVertex(c1)}, shared),
			}
		}, 
		m
	)
end

function m.fromAxisAlignedWedge(position, size, shared)
	local c2, c3 = position + (size*v2), position + (size*v3)
	local c5, c6 = position + (size*v5), position + (size*v6)
	local c7, c8 = position + (size*v7), position + (size*v8)
	return setmetatable(
		{
			["polygons"] = {
				newPolygon({newVertex(c2), newVertex(c5), newVertex(c6)}, shared),
				newPolygon({newVertex(c7), newVertex(c8), newVertex(c3)}, shared),
				newPolygon({newVertex(c6), newVertex(c5), newVertex(c7)}, shared),
				newPolygon({newVertex(c7), newVertex(c5), newVertex(c8)}, shared),
				newPolygon({newVertex(c3), newVertex(c2), newVertex(c7)}, shared),
				newPolygon({newVertex(c7), newVertex(c2), newVertex(c6)}, shared),
				newPolygon({newVertex(c2), newVertex(c3), newVertex(c5)}, shared),
				newPolygon({newVertex(c3), newVertex(c8), newVertex(c5)}, shared),
			}
		}, 
		m
	)
end

function m.fromOrientedWedge(cframe, size, shared) 
	local c2, c3 = (cframe*cf(size*v2)).p, (cframe*cf(size*v3)).p
	local c5, c6 = (cframe*cf(size*v5)).p, (cframe*cf(size*v6)).p
	local c7, c8 = (cframe*cf(size*v7)).p, (cframe*cf(size*v8)).p
	return setmetatable(
		{
			["polygons"] = {
				newPolygon({newVertex(c2), newVertex(c5), newVertex(c6)}, shared),
				newPolygon({newVertex(c7), newVertex(c8), newVertex(c3)}, shared),
				newPolygon({newVertex(c6), newVertex(c5), newVertex(c7)}, shared),
				newPolygon({newVertex(c7), newVertex(c5), newVertex(c8)}, shared),
				newPolygon({newVertex(c3), newVertex(c2), newVertex(c7)}, shared),
				newPolygon({newVertex(c7), newVertex(c2), newVertex(c6)}, shared),
				newPolygon({newVertex(c2), newVertex(c3), newVertex(c5)}, shared),
				newPolygon({newVertex(c3), newVertex(c8), newVertex(c5)}, shared),
			}
		}, 
		m
	)
end

function m.fromAxisAlignedBlock(position, size, shared)
	local c1, c2 = position + (size*v1), position + (size*v2)
	local c3, c4 = position + (size*v3), position + (size*v4)
	local c5, c6 = position + (size*v5), position + (size*v6)
	local c7, c8 = position + (size*v7), position + (size*v8)
	return setmetatable(
		{
			["polygons"] = {
				newPolygon({newVertex(c3), newVertex(c4), newVertex(c2)}, shared),
				newPolygon({newVertex(c2), newVertex(c4), newVertex(c1)}, shared),
				newPolygon({newVertex(c1), newVertex(c5), newVertex(c2)}, shared),
				newPolygon({newVertex(c2), newVertex(c5), newVertex(c6)}, shared),
				newPolygon({newVertex(c6), newVertex(c5), newVertex(c7)}, shared),
				newPolygon({newVertex(c7), newVertex(c5), newVertex(c8)}, shared),
				newPolygon({newVertex(c8), newVertex(c4), newVertex(c7)}, shared),
				newPolygon({newVertex(c7), newVertex(c4), newVertex(c3)}, shared),
				newPolygon({newVertex(c3), newVertex(c2), newVertex(c7)}, shared),
				newPolygon({newVertex(c7), newVertex(c2), newVertex(c6)}, shared),
				newPolygon({newVertex(c4), newVertex(c8), newVertex(c1)}, shared),
				newPolygon({newVertex(c1), newVertex(c8), newVertex(c5)}, shared),
			}
		}, 
		m
	)
end

function m.fromOrientedBlock(cframe, size, shared)
	local c1, c2 = (cframe*cf(size*v1)).p, (cframe*cf(size*v2)).p
	local c3, c4 = (cframe*cf(size*v3)).p, (cframe*cf(size*v4)).p
	local c5, c6 = (cframe*cf(size*v5)).p, (cframe*cf(size*v6)).p
	local c7, c8 = (cframe*cf(size*v7)).p, (cframe*cf(size*v8)).p
	return setmetatable(
		{
			["polygons"] = {
				newPolygon({newVertex(c3), newVertex(c4), newVertex(c2)}, shared),
				newPolygon({newVertex(c2), newVertex(c4), newVertex(c1)}, shared),
				newPolygon({newVertex(c1), newVertex(c5), newVertex(c2)}, shared),
				newPolygon({newVertex(c2), newVertex(c5), newVertex(c6)}, shared),
				newPolygon({newVertex(c6), newVertex(c5), newVertex(c7)}, shared),
				newPolygon({newVertex(c7), newVertex(c5), newVertex(c8)}, shared),
				newPolygon({newVertex(c8), newVertex(c4), newVertex(c7)}, shared),
				newPolygon({newVertex(c7), newVertex(c4), newVertex(c3)}, shared),
				newPolygon({newVertex(c3), newVertex(c2), newVertex(c7)}, shared),
				newPolygon({newVertex(c7), newVertex(c2), newVertex(c6)}, shared),
				newPolygon({newVertex(c4), newVertex(c8), newVertex(c1)}, shared),
				newPolygon({newVertex(c1), newVertex(c8), newVertex(c5)}, shared),
			}
		}, 
		m
	)
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

function m:scission()
	local Polygons = self.polygons
	local PolygonGroups = {}
	local groups = {}
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
			PolygonGroups[GroupIndex] = {PolygonIndex}
		else
			insert(PolygonGroup, PolygonIndex)
		end
		
	end

	if #groups == 1 then return {self} end

	local newcsgs = {}
	
	for i1, v1 in ipairs(PolygonGroups) do
		local NewPolygons = {}
		for i2, v2 in ipairs(v1) do
			NewPolygons[i2] = Polygons[v2]:clone()
		end
		newcsgs[i1] = setmetatable({["polygons"] = NewPolygons}, m)
	end
	
	return newcsgs
end

return m
