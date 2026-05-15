---
name: verify-citations
description: Verify all manuscript and R2R citations resolve to valid bib keys and are contextually appropriate
user-invocable: true
---

# Verify Citations

Check that every `\autocite{}` in the manuscript and R2R resolves to a valid bib key, that no bib entries are orphaned, and that each citation is used in an appropriate context.

## Steps

### 1. Extract all citation keys

Read `manuscript/01_Article_MainText.tex`, `manuscript/02_Article_Supplementary.tex`, and `manuscript/response_to_reviewers_body.tex`. Extract every `\autocite{...}` key (handling comma-separated multi-cites). Deduplicate and sort.

### 2. Extract all bib keys

Read `manuscript/manuscript-bib.bib` and extract every `@article{key,`, `@book{key,`, `@inproceedings{key,`, etc. entry key.

### 3. Cross-reference

- **Missing bib entries**: citation keys used in .tex files that don't exist in the .bib file.
- **Orphaned bib entries**: bib keys that are never cited in any .tex file.
- **R2R-only citations**: keys cited only in `response_to_reviewers_body.tex` but not in the manuscript. These are fine (refsection isolates them) but should be flagged for awareness.
- **Manuscript-only citations**: keys cited only in the manuscript but not in the R2R. This is normal; just report for completeness.

### 4. Context check

For each citation in the manuscript body text, read the surrounding sentence and the bib entry's title/author/year. Flag any citations where:
- The bib entry topic appears unrelated to the citing sentence's context
- The citation appears to be a copy-paste error or placeholder
- A citation is used in a `\changed{}` block but the bib entry doesn't exist

Focus on `\changed{}` blocks since these are new additions most likely to have errors.

### 5. Report

Output three tables:

```
=== Missing Bib Entries ===
| Key | File | Line |

=== Orphaned Bib Entries ===
| Key | Title |

=== R2R-Only Citations ===
| Key | Title |
```

Then a contextual review:
```
=== Citation Context Check ===
| Key | Citing Context (truncated) | Bib Title | Relevant? |
```

Flag any mismatches. If everything checks out, confirm full alignment.

## Important Notes
- Handle comma-separated `\autocite{key1, key2}` correctly
- Also check `\autoref{}` and `\ref{}` for figure/table labels (not bib, but worth noting broken refs)
- The R2R is wrapped in a biblatex `refsection`, so R2R-only cites are expected and OK
- DOI-style keys (e.g., `10.1007/s00040-013-0290-x`) are valid bib keys
