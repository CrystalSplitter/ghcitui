# Contributing

## Contributing as a User

We're open to contributions! You can help by filing issues and making
pull requsets. Comments, contributions, and any other forms of communication
within this project must abide by the
[Contributor Covenant Code of Conduct](#contributor-covenant-code-of-conduct).

### I Found A Bug! Where Do I Report It?

That's great! Please check our [known issues] so far before filing one. If your bug isn't
already there, please file it [here](new-bug)!

### I Want a Feature! Where Do I Request It?

Please file a feature request [here][new-issue]! We may
not be able to add the feature due to low resources, but it's good to have a record of what users
want.

## Contributing as a Developer

Always happy to have more collaborators on this project! Check out the [issues][known issues]
to look for any open bugs or feature requests. If you intend to work on one, please add a comment
to it saying you are.

Also please check out the coding style, and make sure that you abide by the Contributor Convenant
Code of Conduct.

## Coding Style

This project is auto-formatted currently by Fourmolu, with settings specified in
[fourmolu.yaml](./fourmolu.yaml). This is subject to change.

This project roughly tries to follow the
[Kowainik Style guide](https://kowainik.github.io/posts/2019-02-06-style-guide).
Though there are some notable diverges, some of which are listed below.

### Divergences

- Try to limit lines to 100 characters, but no _hard_ limit.
- Let the auto-formatter handle spacing/alignment.

## Profiling

GHCiTUI has some fairly strict runtime requirements, as it needs to have a
snappy response time to events. Therefore, it's important to profile
large changes and report on that profiling.

The `profiteur` tool (https://github.com/jaspervdj/profiteur) does an
excellent job of visualising these profiles.

# Contributor Covenant Code of Conduct

## Our Pledge

We as members, contributors, and leaders pledge to make participation in our
community a harassment-free experience for everyone, regardless of age, body
size, visible or invisible disability, ethnicity, sex characteristics, gender
identity and expression, level of experience, education, socio-economic status,
nationality, personal appearance, race, caste, color, religion, or sexual
identity and orientation.

We pledge to act and interact in ways that contribute to an open, welcoming,
diverse, inclusive, and healthy community.

## Our Standards

Examples of behavior that contributes to a positive environment for our
community include:

- Demonstrating empathy and kindness toward other people
- Being respectful of differing opinions, viewpoints, and experiences
- Giving and gracefully accepting constructive feedback
- Accepting responsibility and apologizing to those affected by our mistakes,
  and learning from the experience
- Focusing on what is best not just for us as individuals, but for the overall
  community

Examples of unacceptable behavior include:

- The use of sexualized language or imagery, and sexual attention or advances of
  any kind
- Trolling, insulting or derogatory comments, and personal or political attacks
- Public or private harassment
- Publishing others' private information, such as a physical or email address,
  without their explicit permission
- Other conduct which could reasonably be considered inappropriate in a
  professional setting

## Enforcement Responsibilities

Community leaders are responsible for clarifying and enforcing our standards of
acceptable behavior and will take appropriate and fair corrective action in
response to any behavior that they deem inappropriate, threatening, offensive,
or harmful.

Community leaders have the right and responsibility to remove, edit, or reject
comments, commits, code, wiki edits, issues, and other contributions that are
not aligned to this Code of Conduct, and will communicate reasons for moderation
decisions when appropriate.

## Scope

This Code of Conduct applies within all community spaces, and also applies when
an individual is officially representing the community in public spaces.
Examples of representing our community include using an official e-mail address,
posting via an official social media account, or acting as an appointed
representative at an online or offline event.

## Enforcement

Instances of abusive, harassing, or otherwise unacceptable behavior may be
reported to the community leaders responsible for enforcement at
`ghcitui [[at]] crystalwobsite [[.]] gay`.
All complaints will be reviewed and investigated promptly and fairly.

All community leaders are obligated to respect the privacy and security of the
reporter of any incident.

## Enforcement Guidelines

Community leaders will follow these Community Impact Guidelines in determining
the consequences for any action they deem in violation of this Code of Conduct:

### 1. Correction

**Community Impact**: Use of inappropriate language or other behavior deemed
unprofessional or unwelcome in the community.

**Consequence**: A private, written warning from community leaders, providing
clarity around the nature of the violation and an explanation of why the
behavior was inappropriate. A public apology may be requested.

### 2. Warning

**Community Impact**: A violation through a single incident or series of
actions.

**Consequence**: A warning with consequences for continued behavior. No
interaction with the people involved, including unsolicited interaction with
those enforcing the Code of Conduct, for a specified period of time. This
includes avoiding interactions in community spaces as well as external channels
like social media. Violating these terms may lead to a temporary or permanent
ban.

### 3. Temporary Ban

**Community Impact**: A serious violation of community standards, including
sustained inappropriate behavior.

**Consequence**: A temporary ban from any sort of interaction or public
communication with the community for a specified period of time. No public or
private interaction with the people involved, including unsolicited interaction
with those enforcing the Code of Conduct, is allowed during this period.
Violating these terms may lead to a permanent ban.

### 4. Permanent Ban

**Community Impact**: Demonstrating a pattern of violation of community
standards, including sustained inappropriate behavior, harassment of an
individual, or aggression toward or disparagement of classes of individuals.

**Consequence**: A permanent ban from any sort of public interaction within the
community.

## Attribution

This Code of Conduct is adapted from the [Contributor Covenant][homepage],
version 2.1, available at
[https://www.contributor-covenant.org/version/2/1/code_of_conduct.html][v2.1].

Community Impact Guidelines were inspired by
[Mozilla's code of conduct enforcement ladder][Mozilla CoC].

For answers to common questions about this code of conduct, see the FAQ at
[https://www.contributor-covenant.org/faq][FAQ]. Translations are available at
[https://www.contributor-covenant.org/translations][translations].

[known issues]: https://github.com/CrystalSplitter/ghcitui/issues
[new-bug]: https://github.com/CrystalSplitter/ghcitui/issues/new?assignees=&labels=bug&projects=&template=bug_report.md&title=%5BBug+Placeholder+Name%5D
[new-issue]: https://github.com/CrystalSplitter/ghcitui/issues/new
[homepage]: https://www.contributor-covenant.org
[v2.1]: https://www.contributor-covenant.org/version/2/1/code_of_conduct.html
[Mozilla CoC]: https://github.com/mozilla/diversity
[FAQ]: https://www.contributor-covenant.org/faq
[translations]: https://www.contributor-covenant.org/translations
