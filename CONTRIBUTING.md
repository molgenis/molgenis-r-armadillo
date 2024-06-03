# Contributing

(tbd)

## Conventional commits

We use [Convertional commits](https://www.conventionalcommits.org/) for our commit messages.

When fixing bugs or adding features we need you to state what it is in one of your commit message:

- bug: fixing out of memory
  > This will increment the bug number (third) from the version number 1.2.3 to 1.2.4
- feat: adding new functionality
  > This will increment the minor number (second) from the version number 1.2.3 to 1.3.3
- breaking: comply to new upstream version of X
  > This will increment the major number (first) from the version number 1.2.3 to 1.3.3

## Continues integration

- We use [CircleCI](.circleci/config.yml) to build and test
- We use [GitHub actions](./.github/workflows/pkgdown.yaml) to build our documentation site.

### CircleCI

To prevent triggering by a push to branch gd-pages and gd-pages-dev CircleCI is filtering out push.
