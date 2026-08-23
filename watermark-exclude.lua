local excluded_files = {
  ["part1_empirical/ch07_machine_learning_factor_selection.qmd"] = true,
  ["part1_empirical/ch08_constrained_portfolio_optimization.qmd"] = true,

  ["part2_quantive_riemann/ch06_sabr_model.qmd"] = true,
  ["part2_quantive_riemann/ch07_callable_hull_white.qmd"] = true,
  ["part2_quantive_riemann/ch08_credit_risk_cds.qmd"] = true,
  ["part2_quantive_riemann/ch09_exotic_derivatives_prdc.qmd"] = true,

  ["part3_enterprise_riemann/ch01_centric_parallel_comput.qmd"] = true,
  ["part3_enterprise_riemann/ch02_distribute_comput_jobtask.qmd"] = true,
  ["part3_enterprise_riemann/ch03_impl_enterprise_calc.qmd"] = true
}

local function normalize_path(path)
  return path:gsub("\\", "/")
end

function Pandoc(doc)
  if not FORMAT:match("latex") then
    return doc
  end

  local current_file = nil

  if PANDOC_STATE.input_files and #PANDOC_STATE.input_files > 0 then
    current_file = normalize_path(PANDOC_STATE.input_files[1])
  end

  if not current_file then
    return doc
  end

  local is_excluded = false

  for path, _ in pairs(excluded_files) do
    if current_file:sub(-#path) == path then
      is_excluded = true
      break
    end
  end

  if is_excluded then
    table.insert(
      doc.blocks,
      1,
      pandoc.RawBlock("latex", "\\ExcludeWatermarkOn")
    )

    table.insert(
      doc.blocks,
      pandoc.RawBlock("latex", "\\ExcludeWatermarkOff")
    )
  end

  return doc
end
