local vec3 = Vector3.new
local abs = math.abs
local CFrameFromMatrix = CFrame.fromMatrix

local BasePart = Instance.new("Part")
BasePart.BrickColor = BrickColor.new("Bright green")
BasePart.CanCollide = false
BasePart.Anchored = true

local vertexPart = BasePart:Clone()
vertexPart.Size = Vector3.new(0.1,0.1,0.1)

local wedge = Instance.new("WedgePart")
wedge.Anchored = true
wedge.TopSurface = Enum.SurfaceType.Smooth
wedge.BottomSurface = Enum.SurfaceType.Smooth

local wedgeMesh = Instance.new("SpecialMesh")
wedgeMesh.MeshType = Enum.MeshType.Wedge
wedgeMesh.Scale = Vector3.new(0,1,1)

local m = {}

local function getProperties(Part)
	return {
		["BrickColor"] = Part.BrickColor,
		["Material"] = Part.Material,
	}
end

local function setProperties(Part, Properties)
	for PropertyName, PropertyValue in pairs(Properties) do
		if Part[PropertyName] then
			Part[PropertyName] = PropertyValue
		end
	end
end

local function drawTriangle(a, b, c, parent, w1, w2)
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
	w1.Size = vec3(0, height, abs(ab:Dot(back)))
	w1.CFrame = CFrameFromMatrix((a + b)/2, right, up, back)
	wedgeMesh:Clone().Parent = w1
	w1.Parent = parent
	w2 = w2 or wedge:Clone()
	w2.Size = vec3(0, height, abs(ac:Dot(back)))
	w2.CFrame = CFrameFromMatrix((a + c)/2, -right, up, -back)
	wedgeMesh:Clone().Parent = w2
	w2.Parent = parent
	return w1, w2
end

local function drawLine(a,b)
	local Part = BasePart:Clone()
	Part.Size = Vector3.new(.025, .025, (a-b).magnitude)
	Part.CFrame = CFrame.new((a + b) / 2, a)
	Part.Parent = workspace
	return Part
end

local function drawPoint(a)
	local Part = vertexPart:Clone()
	Part.CFrame = CFrame.new(a)
	Part.Parent = workspace
	return Part
end

function m.Polygons(csg)
	local CSGModel = Instance.new("Model")
	for _, p in ipairs(csg.polygons) do
		local vertices, properties = p.vertices, p.shared
		for j = 3, #vertices do
			local TriModel = Instance.new("Model")
			local TriPart1, TriPart2 = drawTriangle(vertices[1].pos, vertices[j-1].pos, vertices[j].pos, TriModel)
			if properties then
				setProperties(TriPart1, properties) 
				setProperties(TriPart2, properties)
			end
			TriModel.Parent = CSGModel
		end
	end
	return CSGModel
end

function m.NormalsCSG(csg)
	local polygons = csg.polygons
	for index, polygon in ipairs(polygons) do
		local vertices = polygon.vertices
		local numVertices = #vertices
		local normal = vertices[1].normal
		local average = vec3()
		for i = 1, numVertices do
			average = average + vertices[i].pos
		end
		average = average/numVertices
		local part = drawLine(average,(average+(normal*.25)))
		part.Name = index
	end
end

function m.VerticesCSG(csg)
	local polygons = csg.polygons
	for _, polygon in ipairs(polygons) do
		for _, vertex in ipairs(polygon.vertices) do
			drawPoint(vertex.pos)
		end
	end
end

function m.Triangles(solid)
	local solidparent = workspace --Instance.new("Model", workspace)
	local vlist, ilist = solid.vlist, solid.ilist
	for i = 1, #ilist - 2, 3 do
		local i1 = ilist[i]
		local i2 = ilist[i+1]
		local i3 = ilist[i+2]
		local v1 = vlist[i1]
		local v2 = vlist[i2]
		local v3 = vlist[i3] 
		local vtx1 = Vector3.new(v1[1], v1[2], v1[3])
		local vtx2 = Vector3.new(v2[1], v2[2], v2[3])
		local vtx3 = Vector3.new(v3[1], v3[2], v3[3])
		local trimodel = Instance.new("Model", solidparent)
		trimodel.Name = ((i - 1)/3) + 1
		local w1, w2 = drawTriangle(vtx1, vtx2, vtx3, trimodel)
		w1.Name = ((i - 1)/3) + 1
		w2.Name = ((i - 1)/3) + 1
	end
end

function m.NormalsSolid(solid)
	local vlist, ilist = solid.vlist, solid.ilist
	for i = 1, #ilist - 2, 3 do
		local i1 = ilist[i]
		local i2 = ilist[i+1]
		local i3 = ilist[i+2]
		local v1 = vlist[i1]
		local v2 = vlist[i2]
		local v3 = vlist[i3] 
		local vtx1 = Vector3.new(v1[1], v1[2], v1[3])
		local vtx2 = Vector3.new(v2[1], v2[2], v2[3])
		local vtx3 = Vector3.new(v3[1], v3[2], v3[3])
		local normal = Vector3.new(v1[4], v1[5], v1[6])
		--local normal = (vtx2-vtx1):Cross(vtx3-vtx1).unit
		local pos = (vtx1+vtx2+vtx3)/3
		local part = drawLine( pos, (pos + (normal*.25)) )
		part.Name = ((i - 1)/3) + 1
	end
end

function m.VerticesSolid(solid)
	
end

m.setProperties = setProperties
m.getProperties = getProperties

return m
