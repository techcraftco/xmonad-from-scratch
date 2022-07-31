The configuration in this repository assumes XMonad version 0.17. 

You may find that 0.17 is available in the package manager of your OS. For example, Arch and Fedora 36 both had 0.17 at the time of writing. However, many distros still have 0.15 (like Ubuntu and Raspberry Pi OS at the time of writing). 

You can compile 0.17 yourself without too much trouble by following the instructions before

## Ubuntu, Debian and Raspberry Pi OS

The installation has three steps:

1. Install [Haskell Stack](https://haskellstack.org)
2. Checkout XMonad and XMonad Contrib
3. Compile XMonad

### Installing Haskell Stack

To compile on Ubuntu, Debian or Raspberry Pi OS start by installing Stack:

```shell
sudo apt update && sudo apt install haskell-stack
```

You'll likely find that your version of Stack is outdated. To update it run `stack upgrade`. On x86_64 this will download a pre-built binary, on ARM it needs to compile from scratch and requires a few dependencies:

```shell
sudo apt install libncurses5 libnuma-dev llvm
```

During compilation you may see some complaints about LLVM version - I haven't found these to be a problem. 

On low-memory machines, compilation may fail with and out-of-memory error. You can simply restart until the compilation succeeds.

On a Raspberry Pi expect this step to take _a few hours_ so make sure you do this with a stable power supply! It is safe to interrupt the upgrade with Ctrl-C. Running the command again picks up from where it last left off.

The upgraded Stack won't be on your `$PATH` - it's installed into `$HOME/.local/bin`. To add this to your `$PATH`:

```shell
export PATH=$HOME/.local/bin:$PATH
```

To make this persist into new shells and across restarts, you'll want to the above line to your `.bashrc`, `.zshrc` or whatever is the correct configuration file for your shell.

Check that you're getting the correct Stack with `which stack`

### Checkout XMonad and XMonad Contrib

To get the XMonad source code first install Git:

```shell
sudo apt install git
```

We want our XMonad source code to be in the same directory as our XMonad config file: `$HOME/.config/xmonad`, create directory with:

```shell
mkdir -p $HOME/.config/xmonad
```

Then checkout both `xmonad` and `xmonad-contrib`:

```shell
cd $HOME/.config/xmonad
git checkout --branch 0.17.0 https://github.com/xmonad/xmonad
git checkout --branch 0.17.0 https://github.com/xmonad/xmonad-contrib
```

This specifically checks out the 0.17 release branch. You can omit `--branch 0.17` if you want to try against the absolute latest XMonad code.

### Building XMonad

First install the necessary dependencies:

```shell
sudo apt install libx11-dev libxft-dev libxinerama-dev libxrandr-dev libxss-dev
```

Then, with the XMonad code checked out:

```shell
cd $HOME/.config/xmonad
stack init
stack build
```

