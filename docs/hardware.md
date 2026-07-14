# Hardware and data resources

This page records the deployment host and the data mirroring setup
used to serve the production application.

## Deployment host

| Resource | Specification |
| --- | --- |
| Hardware | Synology NAS (model: `<待填>`) |
| Operating system | Synology DSM (version: `<待填>`) |
| Container runtime | Synology Container Manager (Docker Engine 20.10 or newer) |
| Storage volume | `/volume3/cistrome_web/` (Btrfs) |

To fill the two `<待填>` placeholders:

- NAS model: run `dmidecode -s system-product-name` on the host, or
  check DSM Control Panel → Info Center → Model.
- DSM version: DSM main UI → top-right menu → About DSM.

## External data mirror

Processed data are mirrored to the public Hugging Face Hub repository
`TomatoCisRegDB/TomatoCisRegDB` using `rclone` (v1.74.4). The mirror
URL is:

- https://huggingface.co/TomatoCisRegDB/TomatoCisRegDB

`config.R` (top-level) controls whether the running application
sources data from the host filesystem (`use_huggingface = FALSE`) or
from the Hub (`use_huggingface = TRUE`, with a configurable
`repo_id` and `timeout`).
