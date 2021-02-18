# Changelog

## [1.1.1](https://github.com/inaka/katana-code/tree/1.1.1) (2021-02-18)

[Full Changelog](https://github.com/inaka/katana-code/compare/1.1.0...1.1.1)

**Closed issues:**

- Parse non-module files [\#60](https://github.com/inaka/katana-code/issues/60)

**Merged pull requests:**

- Fix \#60: Handle parsing of none-module files [\#61](https://github.com/inaka/katana-code/pull/61) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [1.1.0](https://github.com/inaka/katana-code/tree/1.1.0) (2021-02-02)

[Full Changelog](https://github.com/inaka/katana-code/compare/1.0.3...1.1.0)

**Closed issues:**

- Missing attribute definition elements for ktn\_code:type [\#53](https://github.com/inaka/katana-code/issues/53)
- `ktn\_code:parse\_tree/1` issue while parsing for module attributes [\#43](https://github.com/inaka/katana-code/issues/43)

**Merged pull requests:**

- Compact strings directly when parsing, to preserve original formatting [\#59](https://github.com/inaka/katana-code/pull/59) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix broken CI [\#58](https://github.com/inaka/katana-code/pull/58) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Add GitHub Actions for CI [\#57](https://github.com/inaka/katana-code/pull/57) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Add some missing tree node types [\#56](https://github.com/inaka/katana-code/pull/56) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))

## [1.0.3](https://github.com/inaka/katana-code/tree/1.0.3) (2020-11-25)

[Full Changelog](https://github.com/inaka/katana-code/compare/1.0.2...1.0.3)

**Merged pull requests:**

- Fix escript parsing [\#55](https://github.com/inaka/katana-code/pull/55) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [1.0.2](https://github.com/inaka/katana-code/tree/1.0.2) (2020-11-25)

[Full Changelog](https://github.com/inaka/katana-code/compare/1.0.1...1.0.2)

**Fixed bugs:**

- Improve formatting in stringyfied macro definitions [\#52](https://github.com/inaka/katana-code/issues/52)

**Merged pull requests:**

- Be far more specific on how to stringify stuff [\#54](https://github.com/inaka/katana-code/pull/54) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [1.0.1](https://github.com/inaka/katana-code/tree/1.0.1) (2020-11-19)

[Full Changelog](https://github.com/inaka/katana-code/compare/1.0.0...1.0.1)

**Fixed bugs:**

- Using macros in macro definitions breaks them [\#49](https://github.com/inaka/katana-code/issues/49)
- Remove discrepancies [\#48](https://github.com/inaka/katana-code/issues/48)

**Merged pull requests:**

- \[Fix \#49\] Don't preprocess macros if we're not going to parse them later [\#51](https://github.com/inaka/katana-code/pull/51) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#48 by properly running dialyzer, xref and then linter [\#50](https://github.com/inaka/katana-code/pull/50) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [1.0.0](https://github.com/inaka/katana-code/tree/1.0.0) (2020-11-19)

[Full Changelog](https://github.com/inaka/katana-code/compare/0.2.1...1.0.0)

**Fixed bugs:**

- ktn\_dodger can't parse stringifyied macro arguments [\#41](https://github.com/inaka/katana-code/issues/41)
- ktn\_dodger can't parse macros in specs [\#38](https://github.com/inaka/katana-code/issues/38)
- ktn\_dodger can't parse macros surrounded with parentheses [\#37](https://github.com/inaka/katana-code/issues/37)

**Closed issues:**

- Allow ktn\_dodger to parse escripts [\#42](https://github.com/inaka/katana-code/issues/42)
- We need a way to retrieve macro text verbatim from ktn\_dodger [\#40](https://github.com/inaka/katana-code/issues/40)

**Merged pull requests:**

- Don't parse \(i.e. stringify\) macro definitions by default. [\#47](https://github.com/inaka/katana-code/pull/47) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#41\] Parse stringyfied macros [\#46](https://github.com/inaka/katana-code/pull/46) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Allow ktn\_dodger to parse escripts [\#45](https://github.com/inaka/katana-code/pull/45) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Prettify the stringification a bit [\#44](https://github.com/inaka/katana-code/pull/44) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.2.1](https://github.com/inaka/katana-code/tree/0.2.1) (2019-12-03)

[Full Changelog](https://github.com/inaka/katana-code/compare/0.2.0...0.2.1)

**Merged pull requests:**

- Prepare release 0.2.1 [\#36](https://github.com/inaka/katana-code/pull/36) ([jfacorro](https://github.com/jfacorro))
- \[\#26\] Remove aleppo from .app.src [\#35](https://github.com/inaka/katana-code/pull/35) ([jfacorro](https://github.com/jfacorro))

## [0.2.0](https://github.com/inaka/katana-code/tree/0.2.0) (2019-12-02)

[Full Changelog](https://github.com/inaka/katana-code/compare/0.1.3...0.2.0)

**Closed issues:**

- Published documentation on hexdocs.pm contains private functions [\#30](https://github.com/inaka/katana-code/issues/30)
- ‘Ignored variable is being used’ reported for non-variables [\#26](https://github.com/inaka/katana-code/issues/26)

**Merged pull requests:**

- Bump version to 0.2.0 [\#33](https://github.com/inaka/katana-code/pull/33) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#30\] Don't show private functions in documentation [\#32](https://github.com/inaka/katana-code/pull/32) ([juanbono](https://github.com/juanbono))
- \[\#26\] Drop aleppo and use a forked epp\_dodger [\#31](https://github.com/inaka/katana-code/pull/31) ([jfacorro](https://github.com/jfacorro))

## [0.1.3](https://github.com/inaka/katana-code/tree/0.1.3) (2019-06-24)

[Full Changelog](https://github.com/inaka/katana-code/compare/0.1.2...0.1.3)

**Closed issues:**

- Unknown type `ktn\_code:tree\_node/0` [\#27](https://github.com/inaka/katana-code/issues/27)

**Merged pull requests:**

- Bump Version to 0.1.3 [\#29](https://github.com/inaka/katana-code/pull/29) ([elbrujohalcon](https://github.com/elbrujohalcon))
- fix error with exporting types [\#28](https://github.com/inaka/katana-code/pull/28) ([NobbZ](https://github.com/NobbZ))

## [0.1.2](https://github.com/inaka/katana-code/tree/0.1.2) (2018-06-29)

[Full Changelog](https://github.com/inaka/katana-code/compare/0.1.0...0.1.2)

**Fixed bugs:**

- ktn\_code:type/1 returns 'var' for ?MODULE\_STRING [\#15](https://github.com/inaka/katana-code/issues/15)

**Closed issues:**

- Replace ktn\_xref\_SUITE by ktn\_meta\_SUITE [\#4](https://github.com/inaka/katana-code/issues/4)
- Fulfill the open-source checklist [\#2](https://github.com/inaka/katana-code/issues/2)

**Merged pull requests:**

- Update deps and bump version to 0.1.2 [\#25](https://github.com/inaka/katana-code/pull/25) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Update Dependencies [\#24](https://github.com/inaka/katana-code/pull/24) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Bump Version to 0.1.1 [\#23](https://github.com/inaka/katana-code/pull/23) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Update aleppo to 1.0.1 [\#22](https://github.com/inaka/katana-code/pull/22) ([jfacorro](https://github.com/jfacorro))
- Add API to provide file name for `parse\_tree` [\#21](https://github.com/inaka/katana-code/pull/21) ([seriyps](https://github.com/seriyps))
- Remove dead hipchat link [\#20](https://github.com/inaka/katana-code/pull/20) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.1.0](https://github.com/inaka/katana-code/tree/0.1.0) (2016-06-14)

[Full Changelog](https://github.com/inaka/katana-code/compare/0.0.4...0.1.0)

**Closed issues:**

- Version Bump to 0.1.0 [\#18](https://github.com/inaka/katana-code/issues/18)
- Move from erlang.mk to rebar3 [\#16](https://github.com/inaka/katana-code/issues/16)
- Version Bump 0.0.4 [\#13](https://github.com/inaka/katana-code/issues/13)

**Merged pull requests:**

- \[Close \#18\] version bump to 0.1.0 [\#19](https://github.com/inaka/katana-code/pull/19) ([Euen](https://github.com/Euen))
- \[Close \#16\] rebar3 support [\#17](https://github.com/inaka/katana-code/pull/17) ([Euen](https://github.com/Euen))

## [0.0.4](https://github.com/inaka/katana-code/tree/0.0.4) (2016-04-26)

[Full Changelog](https://github.com/inaka/katana-code/compare/0.0.3...0.0.4)

**Merged pull requests:**

- \[\#13\] Version Bump 0.0.4 [\#14](https://github.com/inaka/katana-code/pull/14) ([davecaos](https://github.com/davecaos))
- Add support for latin1 encoded source files [\#12](https://github.com/inaka/katana-code/pull/12) ([tjarvstrand](https://github.com/tjarvstrand))

## [0.0.3](https://github.com/inaka/katana-code/tree/0.0.3) (2016-04-07)

[Full Changelog](https://github.com/inaka/katana-code/compare/0.0.2...0.0.3)

**Fixed bugs:**

- Add deps to the app.src file [\#10](https://github.com/inaka/katana-code/pull/10) ([elbrujohalcon](https://github.com/elbrujohalcon))

**Merged pull requests:**

- Version Bump to 0.0.3 [\#11](https://github.com/inaka/katana-code/pull/11) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.0.2](https://github.com/inaka/katana-code/tree/0.0.2) (2016-03-30)

[Full Changelog](https://github.com/inaka/katana-code/compare/0.0.1...0.0.2)

**Fixed bugs:**

- katana-code.app.src should be called katana\_code.app.src [\#6](https://github.com/inaka/katana-code/pull/6) ([elbrujohalcon](https://github.com/elbrujohalcon))

**Merged pull requests:**

- \[\#quick\] Add katana\_code.d to .gitignore [\#9](https://github.com/inaka/katana-code/pull/9) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#quick\] Fix project name [\#8](https://github.com/inaka/katana-code/pull/8) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Version Bump to 0.0.2 [\#7](https://github.com/inaka/katana-code/pull/7) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.0.1](https://github.com/inaka/katana-code/tree/0.0.1) (2016-03-01)

[Full Changelog](https://github.com/inaka/katana-code/compare/d4c5b63916ff692ff1fedc3f31787e4cedb70cf8...0.0.1)

**Merged pull requests:**

- Igaray.version bump [\#5](https://github.com/inaka/katana-code/pull/5) ([igaray](https://github.com/igaray))
- Initial commit [\#3](https://github.com/inaka/katana-code/pull/3) ([igaray](https://github.com/igaray))
- Update LICENSE [\#1](https://github.com/inaka/katana-code/pull/1) ([elbrujohalcon](https://github.com/elbrujohalcon))



\* *This Changelog was automatically generated by [github_changelog_generator](https://github.com/github-changelog-generator/github-changelog-generator)*
