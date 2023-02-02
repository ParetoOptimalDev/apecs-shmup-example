# Quickly running


Assuming you have nix installed, just run:

```sh
nix run github:ParetoOptimalDev/apecs-shmup-example
```

<details>
<summary>click here for nix install instructions otherwise</summary>

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Or see the [Nix download page](https://nixos.org/download.html) for other options if you don't like that one or it somehow manages not to work.

</details>

# Developing


#### with [direnv](https://github.com/direnv/direnv) already installed

```sh
git clone https://github.com/ParetoOptimalDev/apecs-shmup-example.git
cd apecs-shmup-example
, run
```

#### without [direnv](https://github.com/direnv/direnv)

```sh
git clone https://github.com/ParetoOptimalDev/apecs-shmup-example.git
cd apecs-shmup-example
nix develop
, run
```

#### Powers you have in the nix devshell thanks to [srid/haskell-template](https://github.com/srid/haskell-template)

```shell
~/apecs-shmup-example $ ,
Available commands:

## Dev Tools

  , docs  : Start Hoogle server for project dependencies
  , repl  : Start the cabal repl

## Dev Tools 

  , fmt  : Format the source tree

## Primary

  , run  : Run the project with ghcid auto-recompile
```

See the [tips section](https://github.com/srid/haskell-template#tips) on srid/haskell-template for more info.

#### with vscode

See https://srid.ca/haskell-template/start#vscode

# What this is

A nix flake to build the [shmup example from the apecs repo](https://github.com/jonascarpay/apecs/blob/master/examples/Shmup.md).

# Why

Because build issues having to install haskell and system libraries like opengl are a thing of the past thanks to [Nix](https://nixos.org/) :smile:
