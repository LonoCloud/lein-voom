# lein-git-version

A Leiningen plugin to build and use artifacts with particularly
formatted version strings.

## Usage [TL;DR;]

Use this for user-level plugins:

Put `[resolve-git-version "0.1.0-SNAPSHOT"]` into the `:plugins` vector of your
`:user` profile, or if you are on Leiningen 1.x do `lein plugin install
resolve-git-version 0.1.0-SNAPSHOT`.

When building and deploying, insert `git-version` before the task:

    $ lein git-version install

If your project includes git-version-style dependencies, use
`resolve-git-deps` instead of just `deps`:

    $ lein resolve-git-deps

To update your `project.clj` to use the latest versions of your dependencies:

    $ lein freshen-git-deps


## The scenario:

Multiple git projects with one or more leiningen projects in each, all
under active development by a team.

## Use cases:

- Make changes to a project, working with known good (possibly old)
  versions of dependencies from other git repos.

- Test and push a new version of a project with the very latest
  (possibly unreleased) versions of its dependencies.

- Retrieve exactly the source code used to build every component of a
  specific built artifact.

## A solution:

Most approaches addressing these use cases can be grouped into two
categories: ones that use SNAPSHOT artifacts and ones that don't.
SNAPSHOT versions can make some sense if all related deps are kept in
a single git repo, and care is taken to rebuild from sources whenever
they change, though tracking from a built artifact back to a git
commit can still be problematic. Multiple git repos add more problems,
some of which can be addressed by git submodules. Yet, without putting
git commit sha's in artifact version numbers, interoperating with the
larger world of leiningen dependencies is problematic.

Our solution falls into the second category -- using only non-SNAPSHOT
versions for built artifacts and dependency declarations. The most
straightforward way to do this is to change the declared version in
the `project.clj` with every commit. Unfortunately, forgetting to do
this even once leads to an ambiguity about the duplicated version
number that can cause hard-to-discover problems far down the
dependency tree.

### git-version

Instead, we define a command-line modifier `git-version` that causes
the project version seen by any task or plugin to be adjusted to
include a time stamp and short git sha. For example, we might install
our project like so:

    $ lein git-version install

This would cause the installed artifact to have a version looking
something like:

    1.2.3-20120219_223112-g123abcf
    ^-+-^ ^------+------^  ^--+--^
      |          |            \----- git sha
      |          \------------------ year/month/day/hour/minute/second of commit
      \----------------------------- original semantic version from project.clj

This version string includes the original semantic version for
meaningful human consumption. The timestamp is included for a similar
reason, as well as to maintain good sorting order. The git sha is the
piece we need to track back to a specific commit and, therefore, the
source code used to build it. Note that `git-version` will refuse to
operate unless the working copy is clean, to maintain the integrity of
the git sha.

Normally we still use SNAPSHOT as the version declared in the
`project.clj` except perhaps when preparing a real public release, so
that any accidental builds without the use of `git-version` produce a
snapshot version that all tools already know to be imprecise.

We then vigilantly refuse to ever name a SNAPSHOT version in any
dependency list. Note that even third-party dependencies that are
available as git repos can be built with `git-version` to allow
specific-version dependencies instead of imprecise SNAPSHOT deps.

But now when you pull a git version of a project, it may depend on
very specific but unreleased versions of its dependencies. One
solution is to have a continuous integration service set up to
generate, and make available via a maven repo, a jar for every commit
of every project. Such a continuous integration service has other
benefits as well (see "auto-freshen-deps" below), but relying
completely on such a service is not always practical, so another
solution is provided via `resolve-git-versions`


### resolve-git-versions

[Not yet implemented]

This leiningen task works similarly to `deps`, for example:

    $ lein resolve-git-versions

All dependencies of this project will be resolved as normal, including
discovery in user's `.m2` cache, downloaded from Maven Central or
clojars, or from any other wagon defined in the `project.clj`. If all
these fail for any dependency, `resolve-git-versions` will attempt to
fetch via git, build, and install locally that dependency.

It does this by maintaining a directory full of git repos that are
meant to be used only by this plugin. By default these repos are kept
in `.git-version-repos` in the user's home directory. When an artifact
with a git-version needs to be built, the sha is found from among the
git-version-repos (fetching from the upstream git servers if
necessary), and that sha is checked out. The plugin then scans the
`project.clj` files in that git repo to find the one that builds the
needed dependency, and then runs "lein git-version install". Now with
that dependency resolved, normal lein dep resolution is attempted
again, repeating as necessary until all dependencies are resolved.

For now the most common way the process fails is when the required git
repo isn't currently cloned in `.git-version-repos`. Later we may
provide a mechanism for automatic cloning, but for now users must
manually clone the required git repo themselves.


### freshen-git-deps

[Not yet implemented]

One use case not yet addressed is when you are ready to use newer
versions of your dependencies. You can, of course, do this manually by
checking your git repos and constructing the appropriate git-version
string by hand, and editing your `project.clj` appropriately. This may
even be the best approach when using official release versions of
dependencies that don't change too often. But within a team that is
actively developing multiple projects at once, getting the very latest
from other team members should be encouraged for the overall health of
the project. Thus it should be convenient and aspire to be fool proof.
To this end, we provide `freshen-git-deps`:

    $ lein freshen-git-deps

This will scan the dependencies declared in your `project.clj` finding
ones that look like git-versions, and replacing them with the latest
that can be fetched via the git-version-repos directory. The changes
are written directly into your `project.clj`, ready for testing and
eventually committing.

Because this changes a user-editable file, the plugin is very careful
to make only the change needed, leaving all comments, formatting, etc.
untouched. The plugin errs on the side of caution, refusing to make
changes that might break the `project.clj`.

Once the `project.clj` is freshened, you'll want to resolve the git
deps, test everything, fix your code as needed, and push your fixes
and the updated `project.clj` together. This effectively generates a
new version that other projects can reliably depend upon.


### auto freshen-git-deps

[Not yet implemented]

[TBD]


## Drawbacks

- Without a CI system in place, publicly visible project.clj's may
  include deps that aren't available for download anywhere (may be
  partially mitigated by a plugin dep).

- Long, strange-looking version numbers may confuse some users

- Currently supports only git repos


## License

Copyright © 2013 LonoCloud and ViaSat

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
