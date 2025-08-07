# Hangry Squid

A fast-paced social deducation game built on OxCaml + RPCs and inspired by Hunger Games and Squid Games.

## Get Started

Install the latest version of OxCaml at https://oxcaml.org/get-oxcaml/ since this project uses OxCaml to use Bonsai.

After following the installation steps, run `opam switch import requirements.txt` to install the remaining relevant dependencies.

Run `./run.sh` to automatically copy static assets to the build directory and runs the game via dune. Note: it might be necessary to run `chmod +x run.sh` to resolve permission issues.

A server is started at your public IPv4 address. Any user can open a browser tab at `<public-ip-address>:<8080>` and join the game running on that specific server instance.

## Troubleshooting

Adjust the bash script according to any additional assets added. It is recommended it keep the address and port set to `0.0.0.0` and `8080` respectively.
