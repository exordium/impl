# Changelog

## [0.2.0.0] - 2018-11-26
### Added
- Optional methods and some `named` utilities to support them
- Improved example showing heirarchical instance dispatch and optional methods
- Much improved documentation
- Exposed `(++)` type family for appending methods

### Changed
- Methods now has the form `[Required "foo", Optional "bar", ...]` instead of `["foo","bar",...]`
to support optional methods.
- Renamed Named parameter application operator from (!) to ($$) to mintain downstream compatibility

### Removed
- Deprecated `methodsFor` until `reify` is fixed upstream
