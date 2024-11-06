--[[

luaCSG plugin
for converting roblox's CSG instances into luaCSG objects, drawn with WedgeParts

NOTES:
	this csg implementation is simple and not fully optimal. expect large triangle counts, especially on complex unions
	plane epsilon is .005, instead of .001
	uses plugin:Separate(), which is only available for plugins in roblox versions 2015E+
	uses bit32, which is only available in roblox versions 2019M+ (can replace with an equivalent lua 5.1 module)
	
BUGS:
	csg stack overflow on complex unions

]]

local csg do 
	-- didn't want to require() the original module as to fit it all in one lua file, and to exclude any unnecessary parts
	local vec3 = Vector3.new
	local floor = math.floor
	local insert = table.insert
	local bor = bit32.bor -- you can use a lua 5.1 bitwise library as a replacement in pre 2019 roblox

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
		EPSILON = 0.005 --1e-5 -- tolerance used by `splitPolygon()` to decide if a point is on the plane
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
				polygonType = bor(polygonType, ptype)
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
				if (bor(ti, tj) == SPANNING) then
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
		if depth > 8000 then print"luaCSG stack overflow" return end
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

	local sqrt = math.sqrt
	local abs = math.abs
	local sin = math.sin
	local cos = math.cos
	local pi = math.pi
	local clamp = math.clamp

	local cf = CFrame.new

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

	local sqrt = math.sqrt
	function m.fromSphere(position, radius, shared) --radius is math.min(PartSize.x, PartSize.y, PartSize.z)
		local subdivisions = 2
		local phi = (1 + sqrt(5)) / 2
		vlist = {
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
		ilist = {
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

	csg = m
end



local abs = math.abs
local Vector3new = Vector3.new
local CFrameFromMatrix = CFrame.fromMatrix

local wedge = Instance.new("WedgePart")
wedge.Anchored = true
wedge.TopSurface = Enum.SurfaceType.Smooth
wedge.BottomSurface = Enum.SurfaceType.Smooth

local wedgeMesh = Instance.new("SpecialMesh")
wedgeMesh.MeshType = Enum.MeshType.Wedge
wedgeMesh.Scale = Vector3.new(0,1,1)

local function WedgePart(a, b, c, parent, w1, w2)
	local ab, ac, bc = b - a, c - a, c - b
	local abd, acd, bcd = ab:Dot(ab), ac:Dot(ac), bc:Dot(bc)
	if (abd > acd and abd > bcd) then
		c, a = a, c
	elseif (acd > bcd and acd > abd) then
		a, b = b, a
	end
	ab, ac, bc = b - a, c - a, c - b
	local right = ac:Cross(ab).unit
	local up = bc:Cross(right).unit
	local back = bc.unit
	local height = abs(ab:Dot(up))
	w1 = w1 or wedge:Clone()
	w1.Size = Vector3new(0, height, abs(ab:Dot(back)))
	w1.CFrame = CFrameFromMatrix((a + b)/2, right, up, back)
	wedgeMesh:Clone().Parent = w1
	w1.Parent = parent
	w2 = w2 or wedge:Clone()
	w2.Size = Vector3new(0, height, abs(ac:Dot(back)))
	w2.CFrame = CFrameFromMatrix((a + c)/2, -right, up, -back)
	wedgeMesh:Clone().Parent = w2
	w2.Parent = parent
	return w1, w2
end

local function getPartProperties(Part)
	return {
		["BrickColor"] = Part.BrickColor,
		["Material"] = Part.Material,
	}
end

local function setPartProperties(Part, Properties)
	for PropertyName, PropertyValue in pairs(Properties) do
		Part[PropertyName] = PropertyValue
	end
end

local function DrawCSG(csg)
	local CSGModel = Instance.new("Model")
	for _, p in ipairs(csg.polygons) do
		local vertices, properties = p.vertices, p.shared
		for j = 3, #vertices do
			local TriModel = Instance.new("Model")
			local TriPart1, TriPart2 = WedgePart(vertices[1].pos, vertices[j-1].pos, vertices[j].pos, TriModel)
			setPartProperties(TriPart1, properties) setPartProperties(TriPart2, properties)
			for k, v in pairs(properties) do
				if TriPart1[k] then
					TriPart1[k], TriPart2[k] = v, v
				end
			end
			TriModel.Parent = CSGModel
		end
	end
	return CSGModel
end

local v3x, v3y, v3nz = Vector3.new(1,0,0), Vector3.new(0,1,0), Vector3.new(0, 0,-1)

local function isCFrameAxisAligned(cframe)
	return (cframe.rightVector == v3x and cframe.upVector == v3y and cframe.lookVector == v3nz)
end

local function getCSGFromPart(part, Properties)
	if part:IsA("BasePart") then
		Properties = Properties or getPartProperties(part) -- polygon properties to keep track of
		local Shape = part.Shape
		local CF = part.CFrame
		local HalfSize = part.Size*.5
		local isAxisAligned = isCFrameAxisAligned(CF)
		if Shape == Enum.PartType.Ball then
			local radius = math.min(HalfSize.x, HalfSize.y, HalfSize.z)*.5
			return csg.fromSphere(part.Position, radius, Properties)
		elseif Shape == Enum.PartType.Block then
			if isAxisAligned then
				return csg.fromAxisAlignedBlock(part.Position, HalfSize, Properties)
			else	
				return csg.fromOrientedBlock(CF, HalfSize, Properties)
			end
		elseif Shape == Enum.PartType.CornerWedge then
			if isAxisAligned then
				return csg.fromAxisAlignedCornerWedge(part.Position, HalfSize, Properties)
			else	
				return csg.fromOrientedCornerWedge(CF, HalfSize, Properties)
			end
		elseif Shape == Enum.PartType.Cylinder then
			if isAxisAligned then
				return csg.fromOrientedCylinder(CF, HalfSize, Properties) --csg.fromAxisAlignedCylinder(part.Position, HalfSize, Properties)
			else	
				return csg.fromOrientedCylinder(CF, HalfSize, Properties)
			end
		elseif Shape == Enum.PartType.Wedge then
			if isAxisAligned then
				return csg.fromAxisAlignedWedge(part.Position, HalfSize, Properties)
			else	
				return csg.fromOrientedWedge(CF, HalfSize, Properties)
			end
		end
	else

	end
end

local getTreeFromCSG -- create a tree that represents the csg operations
getTreeFromCSG = function(tbl)
	local tree = {}
	for _, v in ipairs(tbl) do
		if v.ClassName == "UnionOperation" or v.ClassName == "NegateOperation" then
			local ClonedInstance = v:Clone() -- cloned in order to preserve the original instance
			local SeparatedInstances = plugin:Separate({ClonedInstance})
			tree[v] = getTreeFromCSG(SeparatedInstances) -- create new child nodes from parent union's separated components
		else
			tree[v] = true -- part, has no child nodes
		end
	end
	return tree
end

local traverseTree -- traverse and sort tree into level order, bottom up
traverseTree = function(tree, levels, csgInstance)
	local level = {csgInstance} -- new level, mark it with which union/negate it belongs to
	for k, v in pairs(tree) do
		if k.ClassName == "UnionOperation" or k.ClassName == "NegateOperation" then
			traverseTree(v, levels, k)
		end
		table.insert(level, k) -- add node to level
	end
	if csgInstance then
		table.insert(levels, level)
	end
end

local function recreateCSG(UnionOperation, DeleteOriginal, UseNegateProperties)
	local csgTree = getTreeFromCSG({UnionOperation}) -- print(csgTree)

	local csgTreeLevels = {}
	traverseTree(csgTree, csgTreeLevels) -- print(csgTreeLevels)

	local csgObjects = {}

	for _, level in ipairs(csgTreeLevels) do -- for each level of the csg tree
		local csgInstance = level[1] -- the negate/union the tree level belongs to
		local csgInstanceClass = csgInstance.ClassName

		local csgObject = nil
		if csgInstanceClass == "UnionOperation" then -- if the tree level is for a union
			local NegateOperations = {}

			for i = 2, #level do -- for each node in the level
				local instance = level[i]
				local instanceClass = instance.ClassName

				if instanceClass == "UnionOperation" then -- if a union node
					csgObject = (csgObject and csgObject:union(csgObjects[instance])) or csgObjects[instance]
				elseif instanceClass == "NegateOperation" then -- if a negate node
					table.insert(NegateOperations, csgObjects[instance])
				elseif instanceClass == "Part" then -- if a part node
					csgObject = (csgObject and csgObject:union(getCSGFromPart(instance))) or getCSGFromPart(instance)
				end -- end if
			end -- end for

			for i = 1, #NegateOperations do -- do the negates after the unions (the lua csg implementation doesn't union negates, only subtracts)
				csgObject = csgObject:subtract(NegateOperations[i])
			end -- end for
		elseif csgInstanceClass == "NegateOperation" then -- if the tree level is for a negate, child is either a union or part
			csgObject = csgObjects[level[2]]
			if csgObject == nil then
				local Properties = (UseNegateProperties == true and getPartProperties(csgInstance)) or nil
				csgObject = getCSGFromPart(level[2], Properties)
			end
		end -- end if

		csgObjects[csgInstance] = csgObject -- pair the roblox csg with the matching custom csg class
	end -- end for

	local Model = DrawCSG(csgObjects[UnionOperation])
	Model.Name = UnionOperation.Name
	Model.Parent = UnionOperation.Parent
	if DeleteOriginal == true then
		UnionOperation:Destroy()
	end
	return Model
end

local function recreateTableOfCSG(tbl, DeleteOriginal, UseNegateProperties)
	local NumTbl = #tbl
	for i = NumTbl, 1 , -1 do
		if tbl[i].ClassName ~= "UnionOperation" then
			table.remove(tbl, i)
		end
	end
	NumTbl = #tbl

	local printInterval = math.floor((NumTbl/100))

	local t0 = tick()
	print("started recreating table of union(s)")
	for index, instance in ipairs(tbl) do
		recreateCSG(instance, DeleteOriginal, UseNegateProperties)
		task.wait(1/60)
		if index % printInterval == 0 then  -- dont print too often
			print(math.floor(((index/NumTbl)*100)+.5).."%")
		end
	end
	local td = tick() - t0
	print("finished recreating "..NumTbl.." union(s) in "..td.." seconds")
end

local function outputHelpInfo()
	print([[
	
	function recreate(UnionOperation, DeleteOriginal, UseNegateProperties)
		recreates the UnionOperation out of WedgeParts
		if DeleteOriginal is true, the original UnionOperation is deleted (default: false)
		if UseNegateProperties is true, the properties from a NegateOperation will be used instead of it's separated part properties (default: false)
		ex: _G.luaCSG.recreate(workspace.Union, true, true)
			
	function recreateTable(tbl, DeleteOriginal, UseNegateProperties)	
		recreates all UnionOperation(s) in tbl (a table containing UnionOperation(s))
		ex:	_G.luaCSG.recreateTable(workspace:GetDescendants(), true, true)	
	
	]])
end

print"luaCSG plugin loaded"
print"type _G.luaCSG.help() in command bar for a list of commands"

_G.luaCSG = {
	["help"] = outputHelpInfo,
	["recreate"] = recreateCSG,
	["recreateTable"] = recreateTableOfCSG,
}
