--[[
(group select and save csg, draw, plugin to file as .rbxm in your plugin folder)

luaCSG plugin
for converting roblox's CSG instances into procmesh objects, drawn with WedgeParts

NOTES:
	this csg implementation is simple and not fully optimal. expect large triangle counts, especially on complex unions
	uses plugin:Separate(), which is only available for plugins in roblox versions 2015E+

]]
local ModuleContainer = script.Parent
local csg = require(ModuleContainer.csg)
local draw = require(ModuleContainer.draw)
local getCSGFromPart = csg.getCSGFromPart
local getProperties = draw.getProperties
local drawCSG = draw.Polygons
local pluginSeparate = plugin.Separate

local ChangeHistoryService = game:GetService("ChangeHistoryService")

local getTreeFromCSG -- create a tree that represents the csg operations
getTreeFromCSG = function(tbl)
	local tree = {}
	for _, instance in ipairs(tbl) do
		if instance.ClassName == "UnionOperation" or instance.ClassName == "NegateOperation" then
			local success, SeparatedInstances = pcall(pluginSeparate, plugin, {instance})
			if success == false then 
				print("plugin:Separate() failed on '"..instance.Name.."'")
				return SeparatedInstances, tree
			end
			local status, branch = getTreeFromCSG(SeparatedInstances) -- create new child nodes from parent union's separated components
			if status ~= true then
				return status, tree
			end
			tree[instance] = branch
		else
			tree[instance] = true -- part, has no child nodes
		end
	end
	return true, tree
end

local traverseTree -- traverse and sort tree into level order, bottom up
traverseTree = function(tree, levels, csgInstance)
	local level = {csgInstance} -- new level, mark it with which union/negate it belongs to
	for instance, branch in pairs(tree) do
		if instance.ClassName == "UnionOperation" or instance.ClassName == "NegateOperation" then
			traverseTree(branch, levels, instance)
		end
		table.insert(level, instance) -- add node to level
	end
	if csgInstance then
		table.insert(levels, level)
	end
end

local cleanUpTree
cleanUpTree = function(tree)
	for instance, branch in pairs(tree) do
		if instance.ClassName == "UnionOperation" or instance.ClassName == "NegateOperation" then
			cleanUpTree(branch)
		end
		tree[instance] = nil
		instance:Destroy()
	end
end

local function recreateCSG(UnionOperation, DeleteOriginal, UseNegateProperties)
	local status, csgTree = getTreeFromCSG({UnionOperation:Clone()})
	
	if status ~= true then
		print(status)
		print("Error creating CSG tree for '"..UnionOperation.Name.."', skipping")
		cleanUpTree(csgTree)
		return
	end
	
	local csgTreeLevels = {}
	traverseTree(csgTree, csgTreeLevels)

	local csgObjects = {}

	for _, level in ipairs(csgTreeLevels) do -- for each level of the csg tree
		local csgInstance = level[1] -- the negate/union the tree level belongs to
		local csgInstanceClass = csgInstance.ClassName

		local csgObject = nil
		if csgInstanceClass == "UnionOperation" then -- if the tree level is for a union
			local NegateOperations, NumNegates = {}, 0

			for i = 2, #level do -- for each node in the level
				local instance = level[i]
				local instanceClass = instance.ClassName

				if instanceClass == "UnionOperation" then -- if a union node
					csgObject =
						(csgObject and csgObject:union(csgObjects[instance])) 
						or csgObjects[instance]
				elseif instanceClass == "NegateOperation" then -- if a negate node
					NumNegates = NumNegates + 1
					NegateOperations[NumNegates] = csgObjects[instance]
				elseif instanceClass == "Part" then -- if a part node
					csgObject = 
						( csgObject and csgObject:union( getCSGFromPart(instance, getProperties(instance)) ) ) 
						or getCSGFromPart(instance, getProperties(instance))
				end -- end if
			end -- end for

			for i = 1, NumNegates do -- do the negates after the unions (the lua csg implementation doesn't union negates, only subtracts)
				csgObject = csgObject:subtract(NegateOperations[i])
			end
		elseif csgInstanceClass == "NegateOperation" then -- if the tree level is for a negate, child is either a union or part
			local child = level[2]
			csgObject = csgObjects[child] or getCSGFromPart( child, (UseNegateProperties and getProperties(csgInstance)) or getProperties(child) )
			--[[
			csgObject = csgObjects[child]
			if csgObject == nil then
				local NegateHasWrongColor, PartHasWrongColor = csgInstance.BrickColor.Name == "Institutional white", child.BrickColor.Name == "Medium stone grey"
				if NegateHasWrongColor and not PartHasWrongColor then
					csgObject = getCSGFromPart(child, getProperties(child))
				elseif not NegateHasWrongColor and PartHasWrongColor then
					csgObject = getCSGFromPart(child, getProperties(csgInstance))
				elseif NegateHasWrongColor and PartHasWrongColor then
					if UseNegateProperties then
						csgObject = getCSGFromPart(child, getProperties(csgInstance))
					else
						csgObject = getCSGFromPart(child, getProperties(child))
					end
				end
			end
			]]
		end -- end if

		csgObjects[csgInstance] = csgObject -- pair the roblox csg with the matching custom csg class
	end -- end for

	local Model = drawCSG(csgObjects[next(csgTree)])
	Model.Name = UnionOperation.Name
	Model.Parent = UnionOperation.Parent
	if DeleteOriginal then
		UnionOperation:Destroy()
	end
	for instance in pairs(csgObjects) do
		csgObjects[instance] = nil
		instance:Destroy()
	end
end

local function recreateTableOfCSG(tbl, DeleteOriginal, UseNegateProperties)
	local NumTbl = #tbl
	for i = NumTbl, 1 , -1 do
		if tbl[i].ClassName ~= "UnionOperation" then
			table.remove(tbl, i)
		end
	end
	NumTbl = #tbl

	local printInterval = math.ceil(NumTbl/100)

	print("started recreating table of union(s)")
	local t0 = tick()
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
		ex: _G.luaCSG.recreate(workspace.Union, false, true)
			
	function recreateTable(tbl, DeleteOriginal, UseNegateProperties)	
		recreates all UnionOperation(s) in tbl (a table containing UnionOperation(s))
		ex:	_G.luaCSG.recreateTable(workspace:GetDescendants(), true, true)	
	
	]])
end

local function repeatTryBeginRecording(name)
	local recording
	local count = 0
	repeat
		recording = ChangeHistoryService:TryBeginRecording(name)
		count = count + 1
	until recording ~= nil or count >= 30
	if recording == nil then
		print("ChangeHistoryService:TryBeginRecording('"..name.."') failed after "..count.." tries")
	end
	return recording
end

local function wrapFunctionForChangeHistory(func, name)
	return function(...)
		local recording = repeatTryBeginRecording(name)
		func(...)
		if recording then ChangeHistoryService:FinishRecording(recording, Enum.FinishRecordingOperation.Commit) end
		--ChangeHistoryService:SetWaypoint(name)
	end
end

print"luaCSG plugin loaded"
print"type _G.luaCSG.help() in command bar for a list of commands"

_G.luaCSG = {
	help = outputHelpInfo,
	recreate = wrapFunctionForChangeHistory(recreateCSG, "luaCSG.recreate"),
	recreateTable = wrapFunctionForChangeHistory(recreateTableOfCSG, "luaCSG.recreateTable"),
}

