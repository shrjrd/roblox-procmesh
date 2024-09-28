local abs = math.abs
local Vector3new = Vector3.new
local CFrameFromMatrix = CFrame.fromMatrix

local wedge = Instance.new("WedgePart")
wedge.Anchored = true
wedge.TopSurface = Enum.SurfaceType.Smooth
wedge.BottomSurface = Enum.SurfaceType.Smooth

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
	w1.Parent = parent
	w2 = w2 or wedge:Clone()
	w2.Size = Vector3new(0, height, abs(ac:Dot(back)))
	w2.CFrame = CFrameFromMatrix((a + c)/2, -right, up, -back)
	w2.Parent = parent
	return w1, w2
end

local function Line(a, b)
	local Part = Instance.new("Part")
	Part.BrickColor = BrickColor.new("")
	Part.Anchored = true
	Part.CanCollide = false
	Part.Size = Vector3.new(.025, .025, (a-b).magnitude)
	Part.CFrame = CFrame.new((a + b) / 2, a) -- part will be "looking" at pos1
	Part.Parent = workspace
	return Part
end

local function Triangles(solid)
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
		local model = Instance.new("Model", workspace)
		model.Name = ((i - 1)/3) + 1
		local w1, w2 = WedgePart(vtx1, vtx2, vtx3, model)
		w1.Name = ((i - 1)/3) + 1
		w2.Name = ((i - 1)/3) + 1
	end
end

local function Normals(solid)
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
		local normal = (vtx2-vtx1):Cross(vtx3-vtx1).unit
		local pos = (vtx1+vtx2+vtx3)/3
		local part = Line(pos,(pos+(normal*.25)))
		part.Name = ((i - 1)/3) + 1
	end
end

return {
	["Triangles"] = Triangles,
	["Normals"] = Normals,
}
