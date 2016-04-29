all

# Rustdoc and some other markdown parsers don't like 2-space list indent.
rule 'MD007', :indent => 4
# Pandoc generates numbered ordered lists.
rule 'MD029', :style => 'ordered'
# Pandoc generates lists with the following configured spacing after the list
# marker.
rule 'MD030', :ul_single => 3, :ol_single => 2, :ul_multi => 3, :ol_multi => 2
# We use a % style header for rustdoc.
exclude_rule 'MD041'
