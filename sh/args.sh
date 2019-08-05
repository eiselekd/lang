
# Option parsing
while [ $# -gt 0 ]; do
    case "$1" in
        --*=*)      a="${1%%=*}"; b="${1#*=}"; shift; set -- "$a" "$b" "$@" ;;
        -h|--help)  usage; exit 0; shift ;;
        -a|--alpha) alpha=true; shift ;;
        -b|--beta)  beta="$2"; shift 2 ;;
        --)         shift; break ;;
        -*)         usage; die "Invalid option: $1" ;;
        *)          break ;;
    esac
done
