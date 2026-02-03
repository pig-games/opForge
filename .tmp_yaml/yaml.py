class YAMLError(Exception):
    pass


def safe_load(text):
    data = {}
    if text is None:
        return data
    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line or line.startswith('#'):
            continue
        if ':' not in line:
            raise YAMLError(f"Invalid line: {raw_line}")
        key, value = line.split(':', 1)
        key = key.strip()
        value = value.strip()
        if not key:
            raise YAMLError(f"Invalid key in line: {raw_line}")
        if value.startswith(('"', "'")) and value.endswith(('"', "'")) and len(value) >= 2:
            value = value[1:-1]
        data[key] = value
    return data
