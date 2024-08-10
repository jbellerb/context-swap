# context-swap

A ~~bad idea~~ command-line tool for modifying EFI boot order and hibernating.

> [!WARNING]
> This is not what hibernation is designed for and puts the system in a fairly
> precarious state.
> 
> **UNDER NO CIRCUMSTANCES** should partitions mounted in an actively hibernating
> operating system be externally mounted or otherwise interacted with. This is
> extremely likely to cause catastrophic data loss. I still need to do a lot more
> testing with this and am not totally sold on the practicality. This is merely
> an experiment!

## Background

As someone who uses multiple programs that only support specific operating
systems, I often find myself needing to swap between OSes while working.
Unfortunately, this usually means needing to save and close everything I'm
working on, reboot the machine, open the boot menu and override the default
boot order, and then pick up on whatever next task that needs to be done. This
process is inconvenient, time consuming, and laborious. As an attempt to "solve"
this issue, I created this tool to configure UEFI to automatically boot the
other operating system and then hibernate. This makes the process much less
tedious and I no longer have to close everything I have open. Keep in mind
hibernation is really slow (although not much slower than fully rebooting). In
addition, I have not found a way to trigger a hibernate and immediate reboot so
you will also need to press the power button once the hibernate finishes.

## OS Support

Support for systems will be added as-needed while I experiment with the concept
more.

| OS | Support Level | Notes |
| --- | --- | --- |
| OpenBSD | Full | 1 |
| Windows | Planned | 2 |
| FreeBSD | Experimental | 3 |
| NetBSD | Experimental | |
| DragonflyBSD | Experimental | 3 |

<sup>1</sup> To modify UEFI variables at runtime, `kern.securelevel` must be set to -1.\
<sup>2</sup> UEFI variables may only be modified on Windows 10 and later.\
<sup>3</sup> The `efirt` driver must be compiled into your kernel or `efirt_load="YES"`
must be set in your `loader.conf`.

## Usage

### Synopsis

**cswap** [**-LnqVvX**] [target]

### Description

The **cswap** utility switches between installed operating systems by
hibernating and configuring the system to boot back into a different partition,
leaving the previous system in a hibernated state that can be resumed later.
This requires modifying UEFI boot variables, so the tool must be run as root.

In addition to the core functionality, **cswap** also allows viewing the current
UEFI boot order and queueing up boot overrides for later.

The options are as follows:

**-L**\
List all known boot targets and their IDs in boot priority order then exit.

**-n**\
Hibernate the system without changing any boot priority. If an override was
previously added using **-q** and hasn't been cleared with **-X**, it will still
take effect.

**-q**\
Change the system boot override without hibernating.

**-V**\
Print the current version then exit.

**-v**\
Enable verbose logging of changes made.

**-X**\
Clear the current boot override then exit.

_target_\
Instead of booting to the next available target, boot to the target specified
by either a numerical ID or a name. IDs may be specified in decimal or hex (with
optional preceding 0x). The given name must exactly match a target. Names are
case-sensitive.

### Examples

Show all available boot targets for choosing a target to swap to.

```bash
$ cswap -L
```

Hibernate and set the system to boot into Windows.

```bash
$ cswap "Windows Boot Manager"
```

Set the system to boot into OpenBSD the next time it turns on. The system will
not be hibernated.

```bash
$ cswap -q "OpenBSD"
```

<br />

#### License

<sup>
Copyright (C) jae beller, 2024.
</sup>
<br />
<sup>
Released under the <a href="https://www.gnu.org/licenses/gpl-3.0.txt">GNU General Public License, Version 3</a> or later. See <a href="LICENSE">LICENSE</a> for more information.
</sup>
