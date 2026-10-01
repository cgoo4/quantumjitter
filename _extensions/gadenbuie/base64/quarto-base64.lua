local puremagic = require("puremagic")


---@param file_path string
---@return string
local function encode_base64(file_path)
  -- Open the file in binary read mode
  local file = io.open(file_path, "rb")
  assert(file, "Could not open file at " .. file_path)

  -- Read the entire file content
  local content = file:read("*all")
  file:close()

  enc = quarto.base64.encode(content)
  assert(enc, "Failed to base64 encode file " .. file_path)

  -- Split the base64 string into lines of 76 characters each
  local lines = {}
  for i = 1, #enc, 76 do
    table.insert(lines, enc:sub(i, i + 75))
  end

  -- Join the lines with newlines
  return table.concat(lines, "\n")
end

local function data_encode_base64(file_path, mime_type)
  local encoded = encode_base64(file_path)

  if not encoded then
    return nil, "Failed to base64 encode file " .. file_path
  end

  if not mime_type then
    mime_type = puremagic.via_path(file_path)
  end


  if mime_type == nil then
    quarto.log.error(
      "Failed to guess mime type for " .. file_path .. ".\n" ..
      "Please supply a mime type as the second argument to `base64-data`. " ..
      "For example, `{{< base64-data 'path/to/file', 'image/png' >}}`.\n" ..
      "See https://www.iana.org/assignments/media-types/media-types.xhtml for a list of mime types."
    )
    assert(mime_type, "Failed to determine mime type")
  end

  return "data:" .. mime_type .. ";base64," .. encoded
end

return {
  ['base64'] = function(args)
    return encode_base64(args[1])
  end,
  ['base64-data'] = function(args)
    if #args < 2 then
      return data_encode_base64(args[1])
    else
      return data_encode_base64(args[1], args[2])
    end
  end
}

