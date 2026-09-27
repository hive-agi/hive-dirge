import socket, subprocess, time, os, sys

def benc(x):
    if isinstance(x, int): return b"i%de" % x
    if isinstance(x, str): x = x.encode()
    if isinstance(x, bytes): return b"%d:%s" % (len(x), x)
    if isinstance(x, list): return b"l" + b"".join(benc(i) for i in x) + b"e"
    if isinstance(x, dict): return b"d" + b"".join(benc(k) + benc(v) for k, v in sorted(x.items())) + b"e"

def bdec(buf, i=0):
    c = buf[i:i+1]
    if c == b"i":
        j = buf.index(b"e", i); return int(buf[i+1:j]), j+1
    if c == b"l":
        i += 1; out = []
        while buf[i:i+1] != b"e":
            v, i = bdec(buf, i); out.append(v)
        return out, i+1
    if c == b"d":
        i += 1; out = {}
        while buf[i:i+1] != b"e":
            k, i = bdec(buf, i); v, i = bdec(buf, i); out[k.decode()] = v
        return out, i+1
    j = buf.index(b":", i); n = int(buf[i:j]); s = buf[j+1:j+1+n]
    return s, j+1+n

class C:
    def __init__(s, port):
        s.sock = socket.create_connection(("127.0.0.1", port)); s.buf = b""; s.n = 0
    def req(s, **kw):
        s.n += 1; kw["id"] = str(s.n); s.sock.sendall(benc(kw)); out = []
        while True:
            try:
                m, k = bdec(s.buf)
                s.buf = s.buf[k:]; out.append(m)
                st = [x.decode() for x in m.get("status", [])]
                if "done" in st: return out
            except (ValueError, IndexError):
                s.buf += s.sock.recv(65536)

def vals(ms):
    return [ (m.get("value") or m.get("err") or b"").decode().strip() for m in ms if "value" in m or "err" in m]

os.makedirs("src/demo", exist_ok=True)
open("src/demo/hot.cljc", "w").write('(ns demo.hot)\n(defn greet [] "v1")\n')
if os.path.exists(".nrepl-port"): os.remove(".nrepl-port")
p = subprocess.Popen(["/home/klein/PP/clojurust/target/release/cljrs", "nrepl", "--port", "0", "--src-path", "src"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
for _ in range(100):
    if os.path.exists(".nrepl-port"): break
    time.sleep(0.1)
port = int(open(".nrepl-port").read().strip())
c = C(port)
print("describe:", [sorted(m.get("ops", {}).keys()) for m in c.req(op="describe")])
print("require+call:", vals(c.req(op="eval", code="(require '[demo.hot :as h]) (h/greet)", session="s1")))
new_src = '(ns demo.hot)\n(defn greet [] "v2-hot")\n'
open("src/demo/hot.cljc", "w").write(new_src)
print("load-file op:", vals(c.req(op="load-file", file=new_src, **{"file-name": "hot.cljc", "file-path": "src/demo/hot.cljc"}, session="s1")))
print("call after load-file (ns user):", vals(c.req(op="eval", code="(in-ns 'user) (demo.hot/greet)", session="s1")))
p.terminate(); p.wait(timeout=5)
