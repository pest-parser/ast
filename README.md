# pest-ast

This is an in-development add-on to the pest parsing library.

Pest-ast provides a structured manner to go from the "dynamically typed" Pest Parse Tree
to a strongly typed (Abstract) Syntax Tree, as well as a derive to do so automatically.
In the future, it's planned to optionally additionally check the source grammar to statically
prevent issues that are currently detected at runtime.

In the _future_ 🦄, pest-ast may provide a way of defining grammar directly on the AST nodes.

## Note:

This crate is actually `from-pest`, which provides the trait framework for the said conversion.
[`pest-ast`](./derive/README.md) provides the actual derive for the conversion.

This README is the root of the repository for legacy reasons. This will be corrected in a future reorganization.

## Contributing

Check out the [issue tracker](https://github.com/pest-parser/ast);
we try to keep it stocked with [good-first-issue](https://github.com/pest-parser/ast/labels/good%20first%20issue)
and [help-wanted](https://github.com/pest-parser/ast/issues?q=is%3Aopen+label%3A%22help+wanted%22) opportunities.
If you have questions, don't be afraid to @ the author (CAD97)
[on Gitter](https://gitter.im/pest-parser/pest) or [on Discord](https://discord.gg/FuPE9JE).
The best thing you can probably do for this library currently is to use it!
More than anything else, I just want eyes on the interface trying it out and seeing where it shines and wher it falters.

## License

pest-deconstruct is licensed under both the MIT license and the Apache License 2.0.
Either terms may be used at your option. All PRs are understood to be agreeing to
contribution under these terms as defined in the Apache license.

See [LICENSE-APACHE] and [LICENSE-MIT] for details.

Copyright 2018 Christopher Durham (aka CAD97)

Dual licensed under the Apache License, Version 2.0 and the MIT License
(collectively, the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

   <http://www.apache.org/licenses/LICENSE-2.0>  
   <https://opensource.org/licenses/MIT>

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
