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


local CSG = {}

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
	self.normal = normal --vec3(normal)
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
			if (ti ~= BACK) then table.insert(f, vi) end
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
-- self.can be used to define per-polygon properties (such as surface color).
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
		self.plane = polygons[1].plane:clone() 
	end
	local front = {}
	local back = {}
	for i, p in ipairs(polygons) do
		self.plane:splitPolygon(p, self.polygons, self.polygons, front, back)
	end
	if depth > 2000 then print("stack overflow") wait() return end -- stack overflow protection
	if (#front > 0) then
		if (not self.front) then self.front = m.Node.new() end
		self.front:build(front, depth + 1)
	end
	if (#back > 0) then
		if (not self.back) then self.back = m.Node.new() end
		self.back:build(back, depth + 1)
	end
end

-- Extra functions

local vec3zero = vec3()

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

local abs = math.abs

local epsilon = 1e-3


local function newTriangle(a, b, c, shared)
	local polygon = newPolygon({newVertex(a,vec3zero), newVertex(b,vec3zero), newVertex(c,vec3zero)}, shared)
	local vertices = polygon.vertices
	local normal = polygon.plane.normal
	vertices[1].normal = normal
	vertices[2].normal = normal
	vertices[3].normal = normal
	return polygon
end

local function FloatFuzzyEq(f1, f2, epsilon)
	return f1 == f2 or abs(f1 - f2) <= (abs(f1) + 1) * epsilon
end

function m.fromOrientedCornerWedge() end
function m.fromOrientedWedge() end

function m.fromOrientedBasePart(cframe, size, shared)
	local c1 = (cframe*CFrame.new(size*v1)).p
	local c2 = (cframe*CFrame.new(size*v2)).p
	local c3 = (cframe*CFrame.new(size*v3)).p
	local c4 = (cframe*CFrame.new(size*v4)).p
	local c5 = (cframe*CFrame.new(size*v5)).p
	local c6 = (cframe*CFrame.new(size*v6)).p
	local c7 = (cframe*CFrame.new(size*v7)).p
	local c8 = (cframe*CFrame.new(size*v8)).p
	local self = setmetatable({}, m)
	self.polygons = {
		newTriangle(c3, c4, c2, shared), newTriangle(c2 ,c4 ,c1, shared),
		newTriangle(c1, c5, c2, shared), newTriangle(c2, c5, c6, shared),
		newTriangle(c6, c5, c7, shared), newTriangle(c7, c5, c8, shared),
		newTriangle(c8, c4, c7, shared), newTriangle(c7, c4, c3, shared),
		newTriangle(c3, c2, c7, shared), newTriangle(c7, c2, c6, shared),
		newTriangle(c4, c8, c1, shared), newTriangle(c1, c8, c5, shared)}
	return self
end

function m.stitchPolygons(csg1, csg2)
	local polygons1, polygons2 = csg1.polygons, csg2.polygons
	local newcsg
	for i = 1, #polygons1 do
		local polygon1, polygon2 = polygons1[i], polygons2[i]
		local vertices1, vertices2 = polygon1.vertices, polygon2.vertices
		local d = ((vertices1[1].pos - vertices2[1].pos).unit):Dot(polygon1.plane.normal)
		if FloatFuzzyEq(d, 0, epsilon) then
		elseif d < 0 then
			local polygons = {polygon1:clone(), polygon2:clone()}
			polygons[1]:flip()
			for i1 = 1, #vertices1 do
				local i2 = (i1 == #vertices1 and 1) or i1 + 1
				local midA, midB = vertices2[i1].pos, vertices1[i2].pos
				insert(polygons, newTriangle(midA, vertices1[i1].pos, midB))
				insert(polygons, newTriangle(midA, midB, vertices2[i2].pos))
			end
			newcsg = (newcsg and newcsg:union(setmetatable({["polygons"] = polygons}, m))) or setmetatable({["polygons"] = polygons}, m)
		elseif d > 0 then
			local polygons = {polygon1:clone(), polygon2:clone()}
			polygons[2]:flip()
			for i1 = 1, #vertices1 do
				local i2 = (i1 == #vertices1 and 1) or i1 + 1
				local midA, midB = vertices2[i1].pos, vertices1[i2].pos
				insert(polygons, newTriangle(midB, vertices1[i1].pos, midA))
				insert(polygons, newTriangle(vertices2[i2].pos, midB, midA))
			end
			newcsg = (newcsg and newcsg:union(setmetatable({["polygons"] = polygons}, m))) or setmetatable({["polygons"] = polygons}, m)
		end
	end
	return newcsg
end

function m:getPolygonsFacingDirection(direction)
	local other = self:clone()
	local polygons = other.polygons
	for i = #polygons, 1, -1 do
		local d = polygons[i].plane.normal:Dot(direction)
		if d <= 0 or FloatFuzzyEq(d, 0, epsilon) then
			table.remove(polygons, i)
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
		if d <= 0 or FloatFuzzyEq(d, 0, epsilon) then
			table.remove(polygons, i)
		end
	end
	return other
end

function m:offsetVertices(vector)
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

function m:offsetVerticesFromPosition(position, maxdistance)
	local other = self:clone()
	local polygons = other.polygons
	for i1 = 1, #polygons do
		local v1 = polygons[i1].vertices
		for i2 = 1, #v1 do
			local v2 = v1[i2]
			local pos = v2.pos
			local dir = pos - position
			local dist = math.clamp(maxdistance - dir.magnitude, 0, maxdistance)
			v2.pos = pos + (dir.unit*dist)
		end
	end
	return other
end

function m:offsetVerticesTowardsPosition()
	
end

function m:scission()
	
end

return m
