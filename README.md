# pest-deconstruct

(Working title, also considered: `pest-ast`, `from-pest`, just making this part of `pest`)

Pest-deconstruct provides a structured manner to go from the "dynamically typed" Pest Parse Tree
to a strongly typed (Abstract) Syntax Tree, as well as a derive to do so automatically.
In the future, it's planned to optionally additionally check the source grammar to statically
prevent issues that are currently detected at runtime.

In the _future_ 🦄, pest-ast may provide a way of defining grammar directly on the AST nodes.

This library is now dogfooded for the [nafi](https://github.com/nafi-lang/rust-nafi) programming language.

## Contributing

See the current list of [[good first issue] issues](
https://github.com/pest-parser/pest_deconstruct/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22).
If you have questions, don't be afraid to @ the author (CAD97) on the [pest Gitter channel](
https://gitter.im/dragostis/pest).


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
