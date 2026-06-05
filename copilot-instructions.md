# ShapeToolKit Copilot Instructions

## Scope
- Treat the package as `R CMD check` first. Prefer changes that keep namespace loading, examples, and documentation clean.
- Make the smallest safe edit that fixes the issue, then verify it before broad refactors.

## Source Of Truth
- Treat roxygen comments in `R/` as the source of truth for `NAMESPACE` and `man/`.
- Do not hand-edit generated files unless there is no other way to unblock a check failure.
- After changing roxygen, regenerate documentation and namespace artifacts.

## Namespace And Imports
- When adding an S3 method, make sure the corresponding generic is imported from the host package.
- Do not rely on an unimported generic being available at namespace load time.
- Keep runtime dependencies in `Imports` and only use `Suggests` for test-only, vignette-only, or optional paths.

## Check Safety
- Package code must load cleanly with only its stated dependencies.
- Prefer `requireNamespace()` guards for optional packages used in non-core paths.
- Do not introduce load-time side effects, global assignments, or example code that depends on undeclared objects.
- Keep example code runnable in a clean check environment, or mark it `\dontrun{}` / `\donttest{}` only when that is truly necessary.

## ASCII And Documentation
- Use ASCII in source code, roxygen, examples, and check-sensitive labels unless a Unicode character is essential.
- Avoid fancy punctuation such as en dashes, em dashes, multiplication signs, Unicode minus signs, and math symbols in code or roxygen.
- If a user-facing doc truly needs Unicode, prefer an explicit ASCII alternative nearby so package checks stay stable.

## Tests
- Put behavioral coverage in `tests/testthat` rather than relying on ad hoc scripts.
- Update or add tests when changing validation, dependency handling, example assumptions, or output structure.
- Keep standalone smoke-test scripts lightweight and do not treat them as a substitute for automated tests.

## Gap Detection Rules
- Keep `R/gap_detection.R` check-safe: no Unicode punctuation in roxygen or messages that can trigger non-ASCII warnings.
- Keep `detect_morphospace_gaps()` examples self-contained and check-safe.
- Prefer explicit ASCII wording such as `0-1`, `x`, `+/-`, and `1-gap` in docs and labels.

## Haug Panel Rules
- Keep `Haug_panel()` examples self-contained and based on data that exists in a fresh package install.
- If the function requires `patchwork`, declare it as a runtime dependency.
- Avoid example references to placeholder objects that only exist in a developer workspace.

## Workflow
- Make the code change first, then regenerate docs, then run the smallest useful validation.
- Verify load errors and examples before spending time on broader warnings.
- Preserve existing style and public APIs unless a fix requires a deliberate change.