-- watermark-exclude.lua
--
-- LagrangeFinance 研修対象外章ウォーターマーク
--
-- 対象:
--   I-7, I-8
--   II-6, II-7, II-8, II-9
--   III-1 ～ III-7

local excluded_titles = {
  ["第I-7章"] = true,
  ["第I-8章"] = true,

  ["第II-6章"] = true,
  ["第II-7章"] = true,
  ["第II-8章"] = true,
  ["第II-9章"] = true,

  ["第III-1章"] = true,
  ["第III-2章"] = true,
  ["第III-3章"] = true,
  ["第III-4章"] = true,
  ["第III-5章"] = true,
  ["第III-6章"] = true,
  ["第III-7章"] = true
}

local function is_excluded_chapter(title)
  for prefix, _ in pairs(excluded_titles) do
    if title:sub(1, #prefix) == prefix then
      return true
    end
  end

  return false
end

function Pandoc(doc)
  if not FORMAT:match("latex") then
    return doc
  end

  local blocks = pandoc.List:new()
  local watermark_on = false

  for _, block in ipairs(doc.blocks) do

    -- Bookの章見出しだけを判定
    if block.t == "Header" and block.level == 1 then
      local title = pandoc.utils.stringify(block.content)
      local excluded = is_excluded_chapter(title)

      if excluded and not watermark_on then
        blocks:insert(
          pandoc.RawBlock(
            "latex",
            "\\ExcludeWatermarkOn"
          )
        )

        watermark_on = true

      elseif not excluded and watermark_on then
        blocks:insert(
          pandoc.RawBlock(
            "latex",
            "\\ExcludeWatermarkOff"
          )
        )

        watermark_on = false
      end
    end

    blocks:insert(block)
  end

  -- 文書末尾では必ず解除
  if watermark_on then
    blocks:insert(
      pandoc.RawBlock(
        "latex",
        "\\ExcludeWatermarkOff"
      )
    )
  end

  doc.blocks = blocks

  return doc
end