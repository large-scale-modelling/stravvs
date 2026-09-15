# stravvs

This repository contains a release of the STRAVVS (Simulating Transacting Rural-Area Ventures in Value Systems) code. STRAVVS is an agent-based model of material flows through business-to-business interactions, the background to which is described in [Polhill et al. (2024)](https://doi.org/10.1007/978-3-031-57785-7_15).

The main code is described in `STRAVVS.nlogo` and associated modularized NetLogo components in the `lib` directory. STRAVVS uses the [`cbr`](https://github.com/DougSalt/cbr) and [`mgr`](https://github.com/garypolhill/netlogo-jvmgr) extensions for NetLogo, of which (possibly out-dated) local copies are provided in the directories with the named extensions.

STRAVVS is released using the [GNU General Public Licence](https://www.gnu.org/licenses/gpl-3.0.en.html); see the `LICENSE` file. If you need a more 'liberal' licence, please contact us.

The `paper_figures`, `system1`, `system2` and `system3` directories contain figures and settings for demonstrating STRAVVS in a journal article that (as at 2026-09-15) is in preparation.

The `CITATION.cff` file contains machine-readable metadata for how to cite STRAVVS. A more human-readable form would be:

[McCormick, B. J. J.](https://orcid.org/0000-0002-8060-0502), [Roxburgh, N.](https://orcid.org/0000-0002-7821-1831) & [Polhill, G.](https://orcid.org/0000-0002-8596-0590) (2026) STRAVVS: A flexible agent-based modelling framework for simulating dynamic value chain networks. _Zenodo_ doi:[10.5281/zenodo.22772889](https://doi.org/10.5281/zenodo.22772889)

[BibTeX](https://bibtex.eu/) (for all versions of STRAVVS):

```bibtex
@misc{stravvs,
  author = {{McCormick}, Benjamin J. J. and Roxburgh, Nick and Polhill, Gary},
  title = {{STRAVVS}: A flexible agent-based modelling framework for simulating dynamic value chain networks},
  howpublished = {Zenodo},
  url = {https://doi.org/10.5281/zenodo.22772889}
}
```
