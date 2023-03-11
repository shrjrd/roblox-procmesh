--- Solids are used to create and manipulate triangle meshes
-- The "solid" here refers to a triangle mesh stored in a table:
--  {
--    vlist = { {0,0,0,_}, {1,2,3,_}, _} -- list of vertices storing data (positions, normals, colors...)
--    ilist = {1, 2, 3, _},              -- flat list of indices; triplets that from the triangles
--    sides = {top = {1, 2, 3, _}, _}    -- shape sides mapped to the list of indices
--    vbuffer = Buffer(),                -- vertex buffer object for rendering, regenerated as needed
--    ibuffer = Buffer(),                -- index buffer object for rendering, regenerated as needed
--  }, with metatable accessors to manipulating functions.

local m = {}
m.__index = m

m.vbuffer_format = {{ type = 'vec3', location = 'VertexPosition' },
                    { type = 'vec3', location = 'VertexNormal'   }}
m.ibuffer_format = {{ type = 'index16' }}


local function listappend(t1, t2) -- mutates t1 in place
  for _,v in ipairs(t2) do
    table.insert(t1, v)
  end
  return t1
end


--- Create solid from existing using a user fn to process each vertex.
-- Preserves all the non-modified vertex information (for example colors).
--    modified_solid = solid_obj:map(function(x,y,z, ...)
--        -- manipulate x,y,z as needed
--        return x, y, z
--      end)
function m:map(fn, side_filter)
  local other = m.new()
  if side_filter then
    for i, vertex in ipairs(self.vlist) do
      other.vlist[i] = {unpack(vertex)}
    end
    for _, i in ipairs(side_filter) do
      local retvals = { fn(unpack(self.vlist[i])) }
      for j, val in ipairs(retvals) do
        other.vlist[i][j] = val
      end
    end
  else
    for i, v in ipairs(self.vlist) do
      other.vlist[i] = { fn(unpack(v)) }
      listappend(other.vlist[i], {select(#other.vlist[i] + 1, unpack(v))})
    end
  end
  for i, index in ipairs(self.ilist) do
    other.ilist[i] = index
  end
  for side, ilist in pairs(self.sides) do
    other.sides[side] = {}
    for i, index in ipairs(ilist) do
      other.sides[side][i] = index
    end
  end
  return other
end


--- Create a solid by transforming each vertex by mat4.
-- Example for laying down an upright mesh:
--    solid:transform(mat4():rotate(pi/2, 1,0,0))
function m:transform(transform, side_filter)
  local tvec3 = vec3()
  return self:map(function(x, y, z)
      return transform:mul(tvec3:set(x, y, z)):unpack()
    end, side_filter)
end


--- Create new solid identical to existing one.
function m:clone()
  local other = m.new()
  for i, vertex in ipairs(self.vlist) do
    other.vlist[i] = {unpack(vertex)}
  end
  for i, index in ipairs(self.ilist) do
    other.ilist[i] = index
  end
  for side, ilist in pairs(self.sides) do
    other.sides[side] = {}
    for i, index in ipairs(ilist) do
      other.sides[side][i] = index
    end
  end
  other.normals_dirty = self.normals_dirty
  return other
end


--- Create solid with flipped vertex order.
-- Reverses the face normals by changing the triangle winding.
function m:flipWinding()
  local other = self:clone()
  for i = 1, #other.ilist, 3 do
    other.ilist[i + 1], other.ilist[i + 2] = other.ilist[i + 2], other.ilist[i + 1]
  end
  return other
end


--- Create solid with x4 the geometry by subdividing each triangle.
-- ABC triangle generates 4 smaller triangles:
--       B---AB---A
--        \  /\  /
--         \/__\/
--        BC\  /CA
--           \/
--           C
function m:subdivide()
  local function halfway(p1, p2)
    return {(p1[1] + p2[1]) / 2, (p1[2] + p2[2]) / 2, (p1[3] + p2[3]) / 2}
  end
  local other = self.new()
  local vertices = {}
  local indices = {}
  for i = 1, #self.ilist, 3 do
    local i1, i2, i3 = self.ilist[i + 0], self.ilist[i + 1], self.ilist[i + 2]
    local va, vb, vc = self.vlist[i1], self.vlist[i2], self.vlist[i3]
    local vab = halfway(va, vb)
    local vbc = halfway(vb, vc)
    local vca = halfway(vc, va)
    listappend(vertices, {va, vab, vca})
    listappend(indices, {#vertices - 2, #vertices - 1, #vertices})
    listappend(vertices, {vab, vb, vbc})
    listappend(indices, {#vertices - 2, #vertices - 1, #vertices})
    listappend(vertices, {vbc, vc, vca})
    listappend(indices, {#vertices - 2, #vertices - 1, #vertices})
    listappend(vertices, {vab, vbc, vca})
    listappend(indices, {#vertices - 2, #vertices - 1, #vertices})
  end
  other.vlist = vertices
  other.ilist = indices
  return other
end


--- Converts flat list of triangle indices to flat list of line indices.
-- Edges shared between triangles are not repeated.
--     eg. {1,2,3,  2,3,4} -> {1,2, 2,3, 3,1, 3,4, 4,2}
function m:triangleToLine()
  local function hash(i1, i2)
    local pair = i1 < i2 and {i1, i2} or {i2, i1}
    return table.concat(pair, ':')
  end
  local line_pairs = {}
  for i = 1, #self.ilist, 3 do
    local i1, i2, i3 = self.ilist[i], self.ilist[i+1], self.ilist[i+2]
    line_pairs[hash(i1, i2)] = {i1, i2}
    line_pairs[hash(i2, i3)] = {i2, i3}
    line_pairs[hash(i3, i1)] = {i3, i1}
  end
  local line_indices = {}
  for _, line_pair in pairs(line_pairs) do
    table.insert(line_indices, line_pair[1])
    table.insert(line_indices, line_pair[2])
  end
  local other = self:clone()
  other.ilist = line_indices
  return other
end


--- Combine triangles from two or more solids into a merged solid.
-- Note that all geometry is preserved (even invisible insides),
-- for better results use the union operator from CSG module.
--    merged = solidA:merge(solidB)
--    merged = solids.merge(solids.new(), unpack(solids_list))
function m:merge(...)
  local other = self:clone()
  for _, another in ipairs({...}) do
    local offset = #other.vlist
    listappend(other.vlist, another.vlist)
    for _, index in ipairs(another.ilist) do
      table.insert(other.ilist, index + offset)
    end
    other.normals_dirty = other.normals_dirty or another.normals_dirty
  end
  return other
end


--- Retrieves a map of connections between vertex indices in a solid.
--     graph = solid:getConnections()
--  The `graph[1][2]` is true if vertices #1 and #2 are connected.
function m:getConnections()
  local graph = {}
  for i = 1, #self.ilist, 3 do
    local ia, ib, ic = self.ilist[i + 0], self.ilist[i + 1], self.ilist[i + 2]
    -- build connectivity graph of ABC triangle with bi-directional lines
    if graph[ia] then graph[ia][ib] = true else graph[ia] = { [ib] = true} end
    if graph[ia] then graph[ia][ic] = true else graph[ia] = { [ic] = true} end
    if graph[ib] then graph[ib][ia] = true else graph[ib] = { [ia] = true} end
    if graph[ib] then graph[ib][ic] = true else graph[ib] = { [ic] = true} end
    if graph[ic] then graph[ic][ia] = true else graph[ic] = { [ia] = true} end
    if graph[ic] then graph[ic][ib] = true else graph[ic] = { [ib] = true} end
  end
  return graph
end


--- Recompute all the triangle normals in a solid, in-place modification!
function m:updateNormals()
  if not self.ilist then
    self.ilist = {}
    for i = 1, #self.vlist do
      self.ilist[i] = i
    end
  end
  local normals = {} -- maps vertex index to list of normals of adjacent faces
  local v1, v2, v3 = vec3(), vec3(), vec3()
  for i = 1, #self.ilist, 3 do
    local vi1, vi2, vi3 = self.ilist[i], self.ilist[i + 1], self.ilist[i + 2]
    v1:set(unpack(self.vlist[vi1]))
    v2:set(unpack(self.vlist[vi2]))
    v3:set(unpack(self.vlist[vi3]))
    local fnormal = {v2:sub(v1):cross(v3:sub(v1)):normalize():unpack()}
    normals[vi1] = normals[vi1] or {}
    normals[vi2] = normals[vi2] or {}
    normals[vi3] = normals[vi3] or {}
    table.insert(normals[vi1], fnormal)
    table.insert(normals[vi2], fnormal)
    table.insert(normals[vi3], fnormal)
  end
  local vnormal, tvec3 = vec3(), vec3()
  for i = 1, #self.vlist do
    assert(normals[i], 'no triangle in index list contains vertex ' .. i)
    vnormal:set(0,0,0)
    local c = 0
    for _, fnormal in ipairs(normals[i]) do
      vnormal:add(tvec3:set(unpack(fnormal)))
      c = c + 1
    end
    vnormal:mul(1 / c)
    local v = self.vlist[i]
    v[4], v[5], v[6] = vnormal:normalize():unpack()
  end
  self.normals_dirty = false
end


--- Draw the solid mesh in the supplied pass.
function m:draw(pass, ...)
  if self.normals_dirty then self:updateNormals() end
  self.vbuffer = self.vbuffer or lovr.graphics.newBuffer(self.vlist, self.vbuffer_format)
  self.ibuffer = self.ibuffer or lovr.graphics.newBuffer(self.ilist, self.ibuffer_format)
  pass:mesh(self.vbuffer, self.ibuffer, ...)
end


--- Draw the wireframe of a solid and each face's normal.
function m:debugDraw(pass, pose, ...)
  local pose = pose or mat4()
  -- wireframe model
  pass:setWireframe(true)
  pass:setColor(0xd35c5c)
  self:draw(pass, pose, ...)
  pass:setWireframe(false)
  -- vertex normals - direct representation
  local tvec3 = vec3()
  local tmat4 = mat4()
  -- face normals - calculated average
  local position, normal = vec3(), vec3()
  for i = 1, #self.ilist, 3 do
    local vi1, vi2, vi3 = self.ilist[i], self.ilist[i + 1], self.ilist[i + 2]
    local v1 = {unpack(self.vlist[vi1])}
    local v2 = {unpack(self.vlist[vi2])}
    local v3 = {unpack(self.vlist[vi3])}
    position:set(         v1[1], v1[2], v1[3])
    position:add(tvec3:set(v2[1], v2[2], v2[3]))
    position:add(tvec3:set(v3[1], v3[2], v3[3]))
    position:mul(1/3)
    position = pose:mul(position)
    normal:set(         v1[4], v1[5], v1[6])
    normal:add(tvec3:set(v2[4], v2[5], v2[6]))
    normal:add(tvec3:set(v3[4], v3[5], v3[6]))
    normal:mul(1/3 * 0.1)
    normal = quat(pose):mul(normal):add(position)
    pass:setColor(0x606060)
    pass:line(position, normal)
    pass:setColor(0x8D1C1C)
    pass:plane(tmat4:set(position, tvec3:set(0.05), quat(normal:sub(position):normalize())))
  end
end

----- Constructors for solid mesh primitives -----

--- Create an empty solid primitive.
function m.new()
  local self = setmetatable({
    vlist = {}, -- vertices
    ilist = {}, -- indices
    sides = {}, -- maps side name to index list (eg. 'top' = {1,2,3,4})
    normals_dirty = true
  }, m)
  return self
end


--- Create the solid shape from CSG representation.
function m.fromCSG(csg)
  local self = m.new()
  for _, p in ipairs(csg.polygons) do
    for j=3,#p.vertices do
      local v = p.vertices[1]
      table.insert(self.vlist, {v.pos.x, v.pos.y, v.pos.z, v.normal.x, v.normal.y, v.normal.z})
      local v = p.vertices[j-1]
      table.insert(self.vlist, {v.pos.x, v.pos.y, v.pos.z, v.normal.x, v.normal.y, v.normal.z})
      local v = p.vertices[j]
      table.insert(self.vlist, {v.pos.x, v.pos.y, v.pos.z, v.normal.x, v.normal.y, v.normal.z})
      table.insert(self.ilist, #self.ilist + 1)
      table.insert(self.ilist, #self.ilist + 1)
      table.insert(self.ilist, #self.ilist + 1)
    end
  end
  return self
end


--- Construct a solid from list of vertices and optional indices.
function m.fromVertices(vertices, indices)
  local self = m.new()
  for i, vertex in ipairs(vertices) do
    self.vlist[i] = {unpack(vertex)}
  end
  if indices then
    for i, index in ipairs(indices) do
      self.ilist[i] = index
    end
  else
    for i = 1, #vertices do
      self.ilist[i] = i
    end
  end
  return self
end


--- Construct a solid from LOVR's Model object.
function m.fromModel(model)
  assert(type(model) == 'userdata' and model.getTriangles, 'unsupported input')
  local vlist_flat, ilist = model:getTriangles()
  local vlist = {}
  for i = 1, #vlist_flat, 3 do
    table.insert(vlist, {vlist_flat[i], vlist_flat[i+1], vlist_flat[i+2]})
  end
  return m.fromVertices(vlist, ilist)
end


--- Single sided 1x1 plane facing down the -Z axis.
function m.quad(subdivisions)
  local size = 1 / math.floor(subdivisions or 1)
  local self = m.new()
  local epsilon = 1e-6
  for y = -0.5, 0.5 - epsilon, size do
    for x = -0.5, 0.5 - epsilon, size do
      table.insert(self.vlist, {x, y, 0})
      table.insert(self.vlist, {x, y + size, 0})
      table.insert(self.vlist, {x + size, y, 0})
      table.insert(self.vlist, {x + size, y + size, 0})
      listappend(self.ilist, {#self.vlist - 3, #self.vlist - 2, #self.vlist - 1})
      listappend(self.ilist, {#self.vlist - 2, #self.vlist - 0, #self.vlist - 1})
    end
  end
  return self
end


--- N-sided equilateral polygon.
function m.ngon(segments)
  segments = segments or 6
  local self = m.new()
  local vic = segments * 2 + 1
  for i = 0, segments - 1 do
    local theta, v1, v2, vi1, vi2
    theta = i * (2 * math.pi) / segments;
    v1 = {0.5 * math.cos(theta),  0.5 * math.sin(theta), 0}
    theta = (i + 1) * (2 * math.pi) / segments;
    v2 = {0.5 * math.cos(theta),  0.5 * math.sin(theta), 0}
    table.insert(self.vlist, v1)
    table.insert(self.vlist, v2)
    vi1, vi2 = #self.vlist - 1, #self.vlist
    listappend(self.ilist, {vic, vi2, vi1})
  end
  table.insert(self.vlist, {0,  0, 0})
  assert(vic, #self.vlist)
  return self
end


--- A cube.
function m.cube()
  local self = m.new()
  local s = 0.5
  self.vlist = {
    {-s, -s, -s}, {-s,  s, -s}, { s, -s, -s}, { s,  s, -s}, -- front
    { s,  s, -s}, { s,  s,  s}, { s, -s, -s}, { s, -s,  s}, -- right
    { s, -s,  s}, { s,  s,  s}, {-s, -s,  s}, {-s,  s,  s}, -- back
    {-s,  s,  s}, {-s,  s, -s}, {-s, -s,  s}, {-s, -s, -s}, -- left
    {-s, -s, -s}, { s, -s, -s}, {-s, -s,  s}, { s, -s,  s}, -- bottom
    {-s,  s, -s}, {-s,  s,  s}, { s,  s, -s}, { s,  s,  s}} -- top
  self.ilist = {
     1,  2,  3,  3,  2,  4, -- front
     5,  6,  7,  7,  6,  8, -- top
     9, 10, 11, 11, 10, 12, -- back
    13, 14, 15, 15, 14, 16, -- bottom
    17, 18, 19, 19, 18, 20, -- left
    21, 22, 23, 23, 22, 24} -- right
  self.sides = {
    right =  {3, 4,  5,  6,  7,  8,  9, 10, 18, 20, 23, 24},
    bottom = {1, 3,  7,  8,  9, 11, 15, 16, 17, 18, 19, 20},
    back =   {6, 8,  9, 10, 11, 12, 13, 15, 19, 20, 22, 24},
    top =    {2, 4,  5,  6, 10, 12, 13, 14, 21, 22, 23, 24},
    front =  {1, 2,  3,  4,  5,  7, 14, 16, 17, 18, 21, 23},
    left =   {1, 2, 11, 12, 13, 14, 15, 16, 17, 19, 21, 22}}
  return self
end


--- A truncated cube (rhombicuboctahedron) with variable slant cutoff.
function m.tcube(slant)
  local self = m.new()
  slant = slant or 0.8
  slant = math.min(math.max(slant, 0), 1)
  local s, l = slant * 0.5, 0.5
  self.vlist = {
    {-s, -l,  s}, {-s, -s,  l}, {-l, -s,  s}, {-s,  s,  l},
    {-s,  l,  s}, {-l,  s,  s}, {-s, -l, -s}, {-l, -s, -s},
    {-s, -s, -l}, {-s,  l, -s}, {-s,  s, -l}, {-l,  s, -s},
    { s, -l,  s}, { l, -s,  s}, { s, -s,  l}, { s,  l,  s},
    { s,  s,  l}, { l,  s,  s}, { s, -l, -s}, { s, -s, -l},
    { l, -s, -s}, { s,  l, -s}, { l,  s, -s}, { s,  s, -l},
  }
  self.ilist = {
    15,  4,  2,   21, 18, 14,   22,  5, 16,    3, 12,  8,
     9, 24, 20,    1,  2,  3,    4,  5,  6,    7,  8,  9,
    10, 11, 12,   13, 14, 15,   16, 17, 18,   19, 20, 21,
    22, 23, 24,    7,  3,  8,    2,  6,  3,    5, 12,  6,
    11,  8, 12,   19,  9, 20,   10, 24, 11,   23, 20, 24,
    13, 21, 14,   22, 18, 23,   17, 14, 18,    1, 15,  2,
    16,  4, 17,    7, 13,  1,   15, 17,  4,   21, 23, 18,
    22, 10,  5,    3,  6, 12,    9, 11, 24,    7,  1,  3,
     2,  4,  6,    5, 10, 12,   11,  9,  8,   19,  7,  9,
    10, 22, 24,   23, 21, 20,   13, 19, 21,   22, 16, 18,
    17, 15, 14,    1, 13, 15,   16,  5,  4,    7, 19, 13,
  }
  self.sides = {
    right =  {13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24},
    bottom = { 1,  2,  3,  7,  8,  9, 13, 14, 15, 19, 20, 21},
    back =   { 1,  2,  3,  4,  5,  6, 13, 14, 15, 16, 17, 18},
    top =    { 4,  5,  6, 10, 11, 12, 16, 17, 18, 22, 23, 24},
    front =  { 7,  8,  9, 10, 11, 12, 19, 20, 21, 22, 23, 24},
    left =   { 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12},
  }
  return self
end


--- Bipyramid with variable number of sides (a diamond shape).
function m.bipyramid(segments)
  local self = m.new()
  segments = segments or 4
  self.sides = {top={}, bottom={}, ring={}}
  for i = 0, segments - 1 do
    -- top half
    table.insert(self.vlist,  {0, 0.5, 0})
    table.insert(self.sides.top, #self.vlist)
    local theta = i * (2 * math.pi) / segments
    local x = 0.5 * math.cos(theta)
    local z = 0.5 * math.sin(theta)
    table.insert(self.vlist, {x, 0, z})
    table.insert(self.sides.ring, #self.vlist)
    theta = (i + 1) * (2 * math.pi) / segments
    x = 0.5 * math.cos(theta)
    z = 0.5 * math.sin(theta)
    table.insert(self.vlist, {x, 0, z})
    table.insert(self.sides.ring, #self.vlist)
    listappend(self.ilist, {#self.vlist, #self.vlist - 1, #self.vlist - 2})
    -- bottom half
    table.insert(self.vlist,  {0, -0.5, 0})
    table.insert(self.sides.bottom, #self.vlist)
    theta = i * (2 * math.pi) / segments
    x = 0.5 * math.cos(theta)
    z = 0.5 * math.sin(theta)
    table.insert(self.vlist, {x, 0, z})
    table.insert(self.sides.ring, #self.vlist)
    theta = (i + 1) * (2 * math.pi) / segments
    x = 0.5 * math.cos(theta)
    z = 0.5 * math.sin(theta)
    table.insert(self.vlist, {x, 0, z})
    table.insert(self.sides.ring, #self.vlist)
    listappend(self.ilist, {#self.vlist, #self.vlist - 2, #self.vlist - 1})
  end
  return self
end


--- A pyramid with variable number of sides.
function m.pyramid(segments)
  local self = m.bipyramid(segments)
  self:transform(mat4(0, -0.5, 0), self.sides.ring)
  listappend(self.sides.bottom, self.sides.ring)
  return self
end


--- A prism with variable number of sides.
function m.cylinder(segments)
  local self = m.new()
  segments = segments or 6
  self.sides = {top={}, bottom={}, ring={}}
  local vTop = segments * 8 + 1
  local vBottom = segments * 8 + 2
  for i = 0, segments - 1 do
    -- ring
    local theta, v1, v2, v3, v4, vi1, vi2, vi3, vi4
    theta = i * (2 * math.pi) / segments;
    v1 = {0.5 * math.cos(theta), -0.5, 0.5 * math.sin(theta)}
    v2 = {0.5 * math.cos(theta),  0.5, 0.5 * math.sin(theta)}
    theta = (i + 1) * (2 * math.pi) / segments;
    v3 = {0.5 * math.cos(theta), -0.5, 0.5 * math.sin(theta)}
    v4 = {0.5 * math.cos(theta),  0.5, 0.5 * math.sin(theta)}
    table.insert(self.vlist, v1)
    table.insert(self.sides.bottom, #self.vlist)
    table.insert(self.vlist, v2)
    table.insert(self.sides.top, #self.vlist)
    table.insert(self.vlist, v3)
    table.insert(self.sides.bottom, #self.vlist)
    table.insert(self.vlist, v4)
    table.insert(self.sides.top, #self.vlist)
    vi1, vi2, vi3, vi4 = #self.vlist-3, #self.vlist-2, #self.vlist-1, #self.vlist
    listappend(self.ilist, {vi1, vi2, vi4, vi1, vi4, vi3})
    -- top and bottom self.sides
    theta = i * (2 * math.pi) / segments;
    v1 = {0.5 * math.cos(theta), -0.5, 0.5 * math.sin(theta)}
    v2 = {0.5 * math.cos(theta),  0.5, 0.5 * math.sin(theta)}
    theta = (i + 1) * (2 * math.pi) / segments;
    v3 = {0.5 * math.cos(theta), -0.5, 0.5 * math.sin(theta)}
    v4 = {0.5 * math.cos(theta),  0.5, 0.5 * math.sin(theta)}
    table.insert(self.vlist, v1)
    table.insert(self.sides.bottom, #self.vlist)
    table.insert(self.vlist, v2)
    table.insert(self.sides.top, #self.vlist)
    table.insert(self.vlist, v3)
    table.insert(self.sides.bottom, #self.vlist)
    table.insert(self.vlist, v4)
    table.insert(self.sides.top, #self.vlist)
    vi1, vi2, vi3, vi4 = #self.vlist-3, #self.vlist-2, #self.vlist-1, #self.vlist
    listappend(self.ilist, {vTop, vi4, vi2, vBottom, vi1, vi3})
  end
  table.insert(self.vlist, {0,  0.5, 0})
  table.insert(self.sides.top, #self.vlist)
  assert(vTop, #self.vlist)
  table.insert(self.vlist, {0, -0.5, 0})
  table.insert(self.sides.bottom, #self.vlist)
  assert(vBottom, #self.vlist)
  return self
end


--- Icosphere with customizable subdivision steps (each is x4 geometry)
-- https://github.com/bjornbytes/lovr-icosphere (MIT License)
function m.sphere(subdivisions)
  local self = m.new()
  subdivisions = subdivisions or 2
  local phi = (1 + math.sqrt(5)) / 2
  self.vlist = {
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
  self.ilist = {
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
      local x = (self.vlist[i][1] + self.vlist[j][1]) / 2
      local y = (self.vlist[i][2] + self.vlist[j][2]) / 2
      local z = (self.vlist[i][3] + self.vlist[j][3]) / 2
      table.insert(self.vlist, { x, y, z })
      splits[key] = #self.vlist
    end

    return splits[key]
  end
  -- Subdivide
  for _ = 1, subdivisions do
    for i = #self.ilist, 1, -3 do
      local v1, v2, v3 = self.ilist[i - 2], self.ilist[i - 1], self.ilist[i - 0]
      local a = split(v1, v2)
      local b = split(v2, v3)
      local c = split(v3, v1)

      table.insert(self.ilist, v1)
      table.insert(self.ilist, a)
      table.insert(self.ilist, c)

      table.insert(self.ilist, v2)
      table.insert(self.ilist, b)
      table.insert(self.ilist, a)

      table.insert(self.ilist, v3)
      table.insert(self.ilist, c)
      table.insert(self.ilist, b)

      table.insert(self.ilist, a)
      table.insert(self.ilist, b)
      table.insert(self.ilist, c)

      table.remove(self.ilist, i - 0)
      table.remove(self.ilist, i - 1)
      table.remove(self.ilist, i - 2)
    end
  end
  -- Normalize
  for _, v in ipairs(self.vlist) do
    local x, y, z = unpack(v)
    local length = math.sqrt(x * x + y * y + z * z) * 2
    v[1], v[2], v[3] = x / length, y / length, z / length
  end
  return self
end


--[[ Octasphere is a geometric primitive in which a sphere is cut into 8
  separate octants which are stitched together with quads. By extruding
  and manipulating quad lengths it is possible to coerce the octasphere
  into various shapes:

    sphere: edges xyz = 0
    box: radii xyz = 0
    circle: radius y = 0, edges xyz = 0
    plane: radii xyz = 0, edge y = 0
    cylinder: radius y = 0, edges xz = 0
    rounded cuboid: radii x = y = z set to small value
    capsule: radii x = y = z, edges xz = 0

  At zero-subdivision level it is also possible to create octagonal and
  hexagonal prisms.

  The surface of octasphere can be divided into these parts:
    8 round "corners" (1/8 of a sphere)
    12 round "edges" (elongated arcs)
    6 flat faces

  A single corner is constructed and mirrored to obtain other seven corners. All eight
  corners are sewn together to form edges. Then the six faces are created on the sides.

  The created octasphere has edge length of 2 and radius of 1; can be resized afterwards.
  Concept and algorithm is based on https://prideout.net/blog/octasphere/ by Philip Rideout.
--]]


--- Create list of indices that sews together two lists of indices.
--          cA       nA               triangle I:  cA nB nA
--        ---*-------*---   edge A    triangle II: cA cB nB
--           | \   I | \
--           |   \   |               Ex. 1-2-3   will generate:
--         \ |II   \ |                   |\|\|    1 4 2, 1 3 4,
--        ---*-------*---   edge B       3-4-6    2 6 3, 2 4 6
--          cB       nB
--  Constructs triangles between edges A and B, defined as list of indices.
--  Similar to triangle strip, but for each triangle all 3 indices will be generated.
--  When supplied, constant offsets can be added to each index fetched from A and B edge.
function m.sewindices(edgeA, edgeB, offsetA, offsetB)
  assert(#edgeA == #edgeB)
  offsetA, offsetB = offsetA or 0, offsetB or 0
  local indices = {}
  for i = 1, #edgeA - 1 do
    local cA, cB = edgeA[i], edgeB[i]
    local nA, nB = edgeA[i + 1], edgeB[i + 1]
    table.insert(indices, cA + offsetA) -- triangle I
    table.insert(indices, nB + offsetB)
    table.insert(indices, nA + offsetA)
    table.insert(indices, cA + offsetA) -- triangle II
    table.insert(indices, cB + offsetB)
    table.insert(indices, nB + offsetB)
  end
  return indices
end


-- Construct a list of vertices inside a geodesic.
local function compute_geodesic(point_a, point_b, num_segments)
  -- add first and last point of geodesic, iterate along arc and compute points
  local tvec3, tquat = vec3(), quat()
  local angle_between_endpoints = math.acos( point_a:dot(point_b) )
  local rotation_axis = vec3(point_a):cross(point_b)
  local first = tvec3:set(point_a)
  local point_list = { { first:unpack() } }
  if num_segments == 0 then
    return point_list
  end
  local dtheta = angle_between_endpoints / num_segments
  for point_index = 1, num_segments - 1 do -- iterate horizontally along slice
    local theta = point_index * dtheta
    local q = tquat:set(theta, rotation_axis.x, rotation_axis.y, rotation_axis.z)
    q:mul(tvec3:set(point_a))
    local point = { tvec3:unpack() }
    table.insert(point_list, point)
  end
  local last = tvec3:set(point_b)
  table.insert(point_list, { last:unpack() })
  return point_list
end



--- Subdivide an octant into vertical slices and construct geodesic of each.
local function tessellate_octasphere_patch(subdivisions)
  local n = 2^subdivisions + 1
  local num_vertices = n * (n + 1) / 2
  local point_a, point_b = vec3(), vec3()
  local vertices = {}
  for i = 0, n - 1 do
    local theta = math.pi * 0.5 * i / (n - 1)
    point_a:set(0,               math.sin(theta), math.cos(theta))
    point_b:set(math.cos(theta), math.sin(theta), 0)
    local num_segments = n - 1 - i
    local geodesic_vertices = compute_geodesic(point_a, point_b, num_segments)
    for _, vertex in ipairs(geodesic_vertices) do
      table.insert(vertices, vertex)
    end
  end
  assert(#vertices == num_vertices)
  -- construct indices for single octasphere patch
  local indices = {}
  local f, j0 = 1, 1
  for col_index = 0, n-2 do
    local col_height = n - 1 - col_index
    local j1 = j0 + 1
    local j2 = j0 + col_height + 1
    local j3 = j0 + col_height + 2
    for row = 0, col_height - 2 do
      table.insert(indices, j0 + row) -- edges and 'odd' inside faces
      table.insert(indices, j1 + row)
      table.insert(indices, j2 + row)
      table.insert(indices, j2 + row) -- 'even' inside faces
      table.insert(indices, j1 + row)
      table.insert(indices, j3 + row)
      f = f + 2
    end
    local row = col_height - 1
    table.insert(indices, j0 + row) -- faces with z = 0
    table.insert(indices, j1 + row)
    table.insert(indices, j2 + row)
    f = f + 1
    j0 = j2
  end
  -- identify indices of vertices on edges
  local edges = {x = {}, y = {}, z = {}} -- edges on x=0, y=0 and z=0 planes
  local step = n
  local xi = 1
  local zi = n
  for i = 1, n do
    table.insert(edges.y, i)
    table.insert(edges.x, xi)
    table.insert(edges.z, zi)
    xi = xi + step
    step = step - 1
    zi = zi + step
  end
  return vertices, indices, edges
end


local function sign(n)
  return (n < 0 and -1) or 1
end


local function reshape_octasphere(solid, rx, ry, rz, ex, ey, ez, slant)
  slant = math.max(-1, math.min(1, slant or 0))
  return solid:map(
    function(x,y,z)
      -- deform the octosphere from initial form (edge = 2, radius = 1)
      x = sign(x) * ex / 2 + (x - sign(x)) / 2 * rx
      y = sign(y) * ey / 2 + (y - sign(y)) / 2 * ry
      z = sign(z) * ez / 2 + (z - sign(z)) / 2 * rz
      -- apply shape slant
      x = x * (1 + (z / (ez / 2 + rz)) * slant)
      y = y * (1 + (z / (ez / 2 + rz)) * slant)
      return x, y, z
    end)
end


function m.octasphere(subdivisions)
  local mergedvertices = {}
  local mergedindices  = {}
  -- create the corner patch (in +X+Y+Z octant) and insert into final shape
  local patchvertices, patchindices, patchedges = tessellate_octasphere_patch(subdivisions)
  -- prepare flipped version of corner patch, to use when mirroring results in wrong winding
  local patchindicesflipped = {}
  for i = 1, #patchindices, 3 do
    patchindicesflipped[i]     = patchindices[i]
    patchindicesflipped[i + 1] = patchindices[i + 2]
    patchindicesflipped[i + 2] = patchindices[i + 1]
  end
  -- rotate vertices of original patch to other 7 corners of octasphere
  local reflections = { -- x   y   z
                  [1] = {  1,  1,  1 },  -- +X+Y+Z (original patch)
                  [2] = {  1,  1, -1 },  -- +X+Y-Z  reflect along z axis
                  [3] = {  1, -1,  1 },  -- +X-Y+Z        6------2
                  [4] = {  1, -1, -1 },  -- +X-Y-Z      / |    / |
                  [5] = { -1,  1,  1 },  -- -X+Y+Z     5------1  |
                  [6] = { -1,  1, -1 },  -- -X+Y-Z     |  8---|--4
                  [7] = { -1, -1,  1 },  -- -X-Y+Z     | /    | /
                  [8] = { -1, -1, -1 }}  -- -X-Y-Z     7------3
  local patchcount = #patchvertices
  local tvec3 = vec3()
  local reflect = vec3()
  for ri, refl in ipairs(reflections) do
    reflect:set(unpack(refl))
    -- reflect/mirror the original patch, insert vertices into final shape
    for _,v in ipairs(patchvertices) do
      local rv = tvec3:set( unpack(v) )
      rv:add(1,1,1) -- spread apart 8 domes, so that no two points are on same coordinates
      rv:mul(reflect)
      local x, y, z = rv:unpack()
      local nv = tvec3:set( unpack(v) )
      nv:mul(reflect)
      local nx, ny, nz = nv:unpack()
      table.insert(mergedvertices, { x, y, z, nx, ny, nz })
    end
    -- create new indices from indices of original patch, with fixed offset
    local offset = (ri - 1) * patchcount

    local indices = refl[1] * refl[2] * refl[3] > 0 and patchindices or patchindicesflipped
    for _,i in ipairs(indices) do
      table.insert(mergedindices, i + offset)
    end
  end
  -- sew the 8 patches together to create rounded edges (will be collapsed for a sphere)
  -- each newly rotated part defines its stitches to existing patches
  -- ordering of elements is important; `rotationstiches` uses same order as `reflections`
  local rotationstiches = {
    {},                                               -- +X+Y+Z  first patch, no sewing needed yet
    {{ 1, 'z', true }},                               -- +X+Y-Z  stitch 2st patch with 1th along z plane, flipped
    {{ 1, 'y', true }},                               -- +X-Y+Z
    {{ 2, 'y' }, { 3, 'z' }},                         -- +X-Y-Z
    {{ 1, 'x' }},                                     -- -X+Y+Z
    {{ 2, 'x', true }, { 5, 'z' }},                   -- -X+Y-Z
    {{ 3, 'x', true }, { 5, 'y' }},                   -- -X-Y+Z
    {{ 4, 'x' }, { 6, 'y', true }, { 7, 'z', true }}} -- -X-Y-Z
  -- sew each new patch together with patches created before it
  for ri, stitchdefs in ipairs(rotationstiches) do
      for _, stitchdef in ipairs(stitchdefs) do
        local ti, axis, flip = unpack(stitchdef)
        local offsetA = (ri - 1) * patchcount
        local offsetB = (ti - 1) * patchcount
        if flip then -- reverse edge order to make the stitch with opposite triangle winding
          offsetA, offsetB = offsetB, offsetA
        end
        local stitch = m.sewindices(patchedges[axis], patchedges[axis], offsetA, offsetB)
        for _, index in ipairs(stitch) do
          table.insert(mergedindices, index)
        end
      end
  end
  -- create 6 faces needed for rounded cuboid (will be collapsed for a sphere and capsule)
  local faces = {  {{0, 2}, {4, 6}, patchedges.x[1]},
                   {{1, 3}, {0, 2}, patchedges.z[1]},
                   {{4, 6}, {5, 7}, patchedges.z[1]},
                   {{5, 7}, {1, 3}, patchedges.x[1]},
                   {{4, 5}, {0, 1}, patchedges.x[#patchedges.x]},
                   {{6, 2}, {7, 3}, patchedges.x[#patchedges.x]}}
  for _, face in ipairs(faces) do
    local edgeA = {face[1][1] * patchcount + face[3], face[1][2] * patchcount + face[3]}
    local edgeB = {face[2][1] * patchcount + face[3], face[2][2] * patchcount + face[3]}
    local stitch = m.sewindices(edgeA, edgeB)
    for _, index in ipairs(stitch) do
      table.insert(mergedindices, index)
    end
  end
  local solid = m.fromVertices(mergedvertices, mergedindices)
  solid.subdivisions = subdivisions
  solid.reshape = reshape_octasphere
  return solid
end


return m
