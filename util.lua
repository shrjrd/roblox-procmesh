local abs = math.abs
local remove = table.remove
local find = table.find
local concat = table.concat
local gmatch = string.gmatch
local insert = table.insert
local vec3 = Vector3.new
local cf = CFrame.new
local _epsilon = 1e-3

local procmesh = workspace.procmesh
local solids = require(procmesh.solids)
local csg = require(procmesh.csg)
local util = {}

local BasePartCorners = {
	{ 1,  1, -1}, --v1 - top front right	
	{ 1, -1, -1}, --v2 - bottom front right	
	{-1, -1, -1}, --v3 - bottom front left	
	{-1,  1, -1}, --v4 - top front left
	{ 1,  1,  1}, --v5 - top back right	
	{ 1, -1,  1}, --v6 - bottom back right	
	{-1, -1,  1}, --v7 - bottom back left
	{-1,  1,  1}} --v8 - top back left

local WedgePartCorners = {}

local CornerWedgePartCorners = {}

local function Vector3ToTable(v3) return {v3.x, v3.y, v3.z} end

function util.GetSolidFromBasePart(part, size)
	size = size or part.Size*.5
	local c = {}
	for i = 1, #BasePartCorners do
		local v = BasePartCorners[i]
		insert(c, Vector3ToTable((part.CFrame * cf(size.x*v[1], size.y*v[2], size.z*v[3])).p))
	end
	local self = solids.new()
	self.vlist = {
		c[3], c[4], c[2], c[1],
		c[1], c[5], c[2], c[6],
		c[6], c[5], c[7], c[8],
		c[8], c[4], c[7], c[3],
		c[3], c[2], c[7], c[6],
		c[4], c[8], c[1], c[5]}
	self.ilist = {
		01,  2,  3,  3,  2,  4, -- front
		05,  6,  7,  7,  6,  8, -- top
		09, 10, 11, 11, 10, 12, -- back
		13, 14, 15, 15, 14, 16, -- bottom
		17, 18, 19, 19, 18, 20, -- left
		21, 22, 23, 23, 22, 24} -- right
	self.sides = {
		right  = {3, 4,  5,  6,  7,  8,  9, 10, 18, 20, 23, 24},
		bottom = {1, 3,  7,  8,  9, 11, 15, 16, 17, 18, 19, 20},
		back   = {6, 8,  9, 10, 11, 12, 13, 15, 19, 20, 22, 24},
		top    = {2, 4,  5,  6, 10, 12, 13, 14, 21, 22, 23, 24},
		front  = {1, 2,  3,  4,  5,  7, 14, 16, 17, 18, 21, 23},
		left   = {1, 2, 11, 12, 13, 14, 15, 16, 17, 19, 21, 22}}
	return self
end

function util.GetCSGFromBasePart(part, size)
	size = size or part.Size*.5
	
	return
end

function ExtrudeTriangleUnder(a1,a2,a3, b1,b2,b3)
	local self = solids.new()
	self.vlist = {Vector3ToTable(a1),Vector3ToTable(a2),Vector3ToTable(a3),Vector3ToTable(b1),Vector3ToTable(b2),Vector3ToTable(b3)}
	self.ilist = {1,2,3, 6,5,4, 1,4,5, 5,2,1, 2,5,6, 6,3,2, 3,6,4, 4,1,3}
	return self
end

function ExtrudeTriangleAbove(a1,a2,a3, b1,b2,b3)
	local self = solids.new()
	self.vlist = {Vector3ToTable(a1),Vector3ToTable(a2),Vector3ToTable(a3),Vector3ToTable(b1),Vector3ToTable(b2),Vector3ToTable(b3)}
	self.ilist = {3,2,1, 4,5,6, 5,4,1, 1,2,5, 6,5,2, 2,3,6, 4,6,3, 3,1,4}
	return self
end

local function NegateSolids(a, b)
	return solids.fromCSG((csg.fromSolid(a)):subtract(csg.fromSolid(b)))
end
util.NegateSolids = NegateSolids

local function UnionSolids(a,b)
	return solids.fromCSG((csg.fromSolid(a)):union(csg.fromSolid(b)))
end
util.UnionSolids = UnionSolids

local function IntersectSolids(a, b)
	return solids.fromCSG((csg.fromSolid(a)):intersect(csg.fromSolid(b)))
end
util.IntersectSolids = IntersectSolids

local function UnionSolidsList(SolidsList)
	local UnionedSolids = SolidsList[1]
	for i = 2, #SolidsList do
		UnionedSolids = UnionSolids(UnionedSolids, SolidsList[i])
	end
	return UnionedSolids
end
util.UnionSolidsList = UnionSolidsList


local function FloatFuzzyEq(f1, f2, epsilon)
	return f1 == f2 or abs(f1 - f2) <= (abs(f1) + 1) * epsilon
end

function util.ExtrudeTriangles(solid, offset)
	local ilist, vlist = solid.ilist, solid.vlist
	local prisms = {}
	for i = 1, #ilist - 2, 3 do -- for every triangle
		local v1, v2, v3 = vlist[ilist[i+0]], vlist[ilist[i+1]], vlist[ilist[i+2]]
		local a1, a2, a3 = vec3(v1[1],v1[2],v1[3]), vec3(v2[1],v2[2],v2[3]), vec3(v3[1],v3[2],v3[3])
		local b1, b2, b3 = a1 + offset, a2 + offset, a3 + offset
		--local n = (a2-a1):Cross(a3-a1).unit
		local d = (offset.unit):Dot((a2-a1):Cross(a3-a1).unit)
		if FloatFuzzyEq(d, 0, _epsilon) then  --extrusion not perpendicular to plane of triangle, not valid
		elseif d > 0 then print"a"
			insert(prisms, ExtrudeTriangleAbove(a1,a2,a3, b1,b2,b3))
		elseif d < 0 then
			print"b"
			insert(prisms, ExtrudeTriangleUnder(a1,a2,a3, b1,b2,b3))
		end
	end
	return UnionSolidsList(prisms)
end

local function InsertTriangleVertices(vlist, ilist, a, b, c)
	local VertexCount = #vlist
	insert(vlist, a)
	insert(ilist, VertexCount+1)
	insert(vlist, b)
	insert(ilist, VertexCount+2)
	insert(vlist, c)
	insert(ilist, VertexCount+3)
end

function util.GetSurfacesTowards(solid, direction)
	-- returns a new solid comprised of surfaces from the original solid that face the specfied direction
	local self = solids.new()
	local vlist, ilist = solid.vlist, solid.ilist
	local selfvlist, selfilist = self.vlist, self.ilist
	for i = 1, #ilist - 2, 3 do
		local v1, v2, v3 = vec3(unpack(vlist[ilist[i+0]])), vec3(unpack(vlist[ilist[i+1]])), vec3(unpack(vlist[ilist[i+2]]))
		local d = ((v2-v1):Cross(v3-v1).unit):Dot(direction)
		if d > 0 and not FloatFuzzyEq(d, 0, _epsilon) then
			InsertTriangleVertices(selfvlist, selfilist, Vector3ToTable(v1), Vector3ToTable(v2), Vector3ToTable(v3))
		end
	end
	return self
end

return util
