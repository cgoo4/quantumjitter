function RawInline (raw)
  if raw.format:match 'html' and raw.text == '<?quarto.version?>'then
    return tostring(quarto.version)
  end
end
