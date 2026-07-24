var mf = Object.defineProperty;
var pf = (n, e, t) => e in n ? mf(n, e, {
	enumerable: true,
	configurable: true,
	writable: true,
	value: t
}) : n[e] = t;
var Fo = (n, e, t) => pf(n, typeof e != "symbol" ? e + "" : e, t);
const yf = {};
const bC = Object.freeze(Object.defineProperty({
	__proto__: null,
	default: yf
}, Symbol.toStringTag, { value: "Module" }));
const xf = "modulepreload";
const Ef = function(n) {
	return "/" + n;
};
const Lh = {};
const OC = function(e, t, i) {
	if (!t || t.length === 0) return e();
	const s = document.getElementsByTagName("link");
	return Promise.all(t.map((r) => {
		r = Ef(r);
		if (r in Lh) return;
		Lh[r] = true;
		const o = r.endsWith(".css"), a = o ? "[rel=\"stylesheet\"]" : "";
		if (!!i) for (let c = s.length - 1; c >= 0; c--) {
			const u = s[c];
			if (s[c].href === r && (!o || s[c].rel === "stylesheet")) return;
		}
		else if (document.querySelector("link[href=\"".concat(r, "\"]").concat(a))) return;
		const h = document.createElement("link");
		h.rel = o ? "stylesheet" : xf;
		if (!o) {
			h.as = "script";
			h.crossOrigin = "";
		}
		h.href = r;
		document.head.appendChild(h);
		if (o) return new Promise((c, u) => {
			h.addEventListener("load", c);
			h.addEventListener("error", () => u(new Error("Unable to preload CSS for ".concat(r))));
		});
	})).then(() => e()).catch((r) => {
		const o = new Event("vite:preloadError", { cancelable: true });
		o.payload = r;
		window.dispatchEvent(o);
		if (!o.defaultPrevented) throw r;
	});
};
const _e = {
	ADD: "add",
	REMOVE: "remove"
};
const xt = { PROPERTYCHANGE: "propertychange" };
const U = {
	CHANGE: "change",
	ERROR: "error",
	BLUR: "blur",
	CLEAR: "clear",
	CONTEXTMENU: "contextmenu",
	CLICK: "click",
	DBLCLICK: "dblclick",
	DRAGENTER: "dragenter",
	DRAGOVER: "dragover",
	DROP: "drop",
	FOCUS: "focus",
	KEYDOWN: "keydown",
	KEYPRESS: "keypress",
	LOAD: "load",
	RESIZE: "resize",
	TOUCHMOVE: "touchmove",
	WHEEL: "wheel"
};
class Tf {
	constructor() {
		this.disposed = false;
	}
	dispose() {
		if (!this.disposed) {
			this.disposed = true;
			this.disposeInternal();
		}
	}
	disposeInternal() {}
}
function Cf(n, e, t) {
	let i, s;
	t = t || Lt;
	let r = 0, o = n.length, a = false;
	for (; r < o;) {
		i = r + (o - r >> 1);
		s = +t(n[i], e);
		s < 0 ? r = i + 1 : (o = i, a = !s);
	}
	return a ? r : ~r;
}
function Lt(n, e) {
	return n > e ? 1 : n < e ? -1 : 0;
}
function Rf(n, e) {
	return n < e ? 1 : n > e ? -1 : 0;
}
function io(n, e, t) {
	if (n[0] <= e) return 0;
	const i = n.length;
	if (e <= n[n.length - 1]) return n.length - 1;
	if (typeof t == "function") {
		for (let s = 1; s < i; ++s) {
			const r = n[s];
			if (n[s] === e) return s;
			if (n[s] < e) return t(e, n[s - 1], n[s]) > 0 ? s - 1 : s;
		}
		return i - 1;
	}
	if (t > 0) {
		for (let s = 1; s < i; ++s) if (n[s] < e) return s - 1;
		return i - 1;
	}
	if (t < 0) {
		for (let s = 1; s < i; ++s) if (n[s] <= e) return s;
		return i - 1;
	}
	for (let s = 1; s < n.length; ++s) {
		if (n[s] == e) return s;
		if (n[s] < e) return n[s - 1] - e < e - n[s] ? s - 1 : s;
	}
	return n.length - 1;
}
function Sf(n, e, t) {
	for (; e < t;) {
		const i = n[e];
		n[e] = n[t];
		n[t] = n[e];
		++e;
		--t;
	}
}
function dt(n, e) {
	const t = Array.isArray(e) ? e : [e], i = t.length;
	for (let s = 0; s < t.length; s++) n[n.length] = t[s];
}
function kt(n, e) {
	const t = n.length;
	if (n.length !== e.length) return false;
	for (let i = 0; i < n.length; i++) if (n[i] !== e[i]) return false;
	return true;
}
function vf(n, e, t) {
	const i = e || Lt;
	return n.every(function(s, r) {
		const o = i(n[r - 1], s);
		return !(o > 0 || t && o === 0);
	});
}
function ni() {
	return true;
}
function Xi() {
	return false;
}
function fs() {}
function Yc(n) {
	let e, t, i;
	return function() {
		const s = Array.prototype.slice.call(arguments);
		if (!t || this !== i || !kt(s, t)) {
			i = this;
			t = s;
			e = n();
		}
		return e;
	};
}
function Zc(n) {
	function e() {
		let t;
		try {
			t = n();
		} catch (i) {
			return Promise.reject(i);
		}
		return t instanceof Promise ? t : Promise.resolve(t);
	}
	return e();
}
function Wi(n) {
	for (const e in n) delete n[e];
}
function si(n) {
	let e;
	for (e in n) return false;
	return !e;
}
class wf {
	constructor(e) {
		this.propagationStopped;
		this.defaultPrevented;
		this.type = e;
		this.target = null;
	}
	preventDefault() {
		this.defaultPrevented = true;
	}
	stopPropagation() {
		this.propagationStopped = true;
	}
}
class Pf extends Tf {
	constructor(e) {
		super();
		this.eventTarget_ = e;
		this.pendingRemovals_ = null;
		this.dispatching_ = null;
		this.listeners_ = null;
	}
	addEventListener(e, t) {
		if (!e || !t) return;
		const i = this.listeners_ || (this.listeners_ = {}), s = i[e] || (i[e] = []);
		if (!s.includes(t)) {
			s.push(t);
		}
	}
	dispatchEvent(e) {
		const t = typeof e == "string", i = t ? e : e.type, s = this.listeners_ && this.listeners_[i];
		if (!s) return;
		const r = t ? new wf(e) : e;
		if (!r.target) {
			r.target = this.eventTarget_ || this;
		}
		const o = this.dispatching_ || (this.dispatching_ = {}), a = this.pendingRemovals_ || (this.pendingRemovals_ = {});
		if (!(i in o)) {
			o[i] = 0;
			a[i] = 0;
		}
		++o[i];
		let l;
		for (let h = 0, c = s.length; h < c; ++h) if ("handleEvent" in s[h] ? l = s[h].handleEvent(r) : l = s[h](r), l === false || r.propagationStopped) {
			l = false;
			break;
		}
		if (--o[i] === 0) {
			let h = a[i];
			for (delete a[i]; h--;) this.removeEventListener(i, fs);
			delete o[i];
		}
		return l;
	}
	disposeInternal() {
		if (this.listeners_) {
			Wi(this.listeners_);
		}
	}
	getListeners(e) {
		return this.listeners_ && this.listeners_[e] || "undefined";
	}
	hasListener(e) {
		return this.listeners_ ? e ? e in this.listeners_ : Object.keys(this.listeners_).length > 0 : false;
	}
	removeEventListener(e, t) {
		if (!this.listeners_) return;
		const i = this.listeners_[e];
		if (!this.listeners_[e]) return;
		const s = this.listeners_[e].indexOf(t);
		if (s !== -1) {
			this.pendingRemovals_ && e in this.pendingRemovals_ ? (this.listeners_[e][s] = fs, ++this.pendingRemovals_[e]) : (this.listeners_[e].splice(s, 1), this.listeners_[e].length === 0 && delete this.listeners_[e]);
		}
	}
}
function Z(n, e, t, i, s) {
	if (s) {
		const o = t;
		t = function(a) {
			n.removeEventListener(e, t);
			return o.call(i != null ? i : this, a);
		};
	} else i && i !== n && (t = t.bind(i));
	const r = {
		target: n,
		type: e,
		listener: t
	};
	n.addEventListener(e, t);
	return r;
}
function wr(n, e, t, i) {
	return Z(n, e, t, i, true);
}
function se(n) {
	if (n && n.target) {
		n.target.removeEventListener(n.type, n.listener);
		Wi(n);
	}
}
class Ls extends Pf {
	constructor() {
		super();
		this.on = this.onInternal;
		this.once = this.onceInternal;
		this.un = this.unInternal;
		this.revision_ = 0;
	}
	changed() {
		++this.revision_;
		this.dispatchEvent(U.CHANGE);
	}
	getRevision() {
		return this.revision_;
	}
	onInternal(e, t) {
		if (Array.isArray(e)) {
			const i = e.length;
			const s = new Array(e.length);
			for (let r = 0; r < e.length; ++r) s[r] = Z(this, e[r], t);
			return s;
		}
		return Z(this, e, t);
	}
	onceInternal(e, t) {
		let i;
		if (Array.isArray(e)) {
			const s = e.length;
			i = new Array(e.length);
			for (let r = 0; r < e.length; ++r) i[r] = wr(this, e[r], t);
		} else i = wr(this, e, t);
		t.ol_key = i;
		return i;
	}
	unInternal(e, t) {
		const i = t.ol_key;
		if (t.ol_key) If(t.ol_key);
		else if (Array.isArray(e)) for (let s = 0, r = e.length; s < r; ++s) this.removeEventListener(e[s], t);
		else this.removeEventListener(e, t);
	}
}
Ls.prototype.on;
Ls.prototype.once;
Ls.prototype.un;
function If(n) {
	if (Array.isArray(n)) for (let e = 0, t = n.length; e < t; ++e) se(n[e]);
	else se(n);
}
function z() {
	throw new Error("Unimplemented abstract method.");
}
let Ff = 0;
function O(n) {
	return n.ol_uid || (n.ol_uid = String(++Ff));
}
class Ah extends wf {
	constructor(e, t, i) {
		super(e);
		this.key = t;
		this.oldValue = i;
	}
}
class Lf extends Ls {
	constructor(e) {
		super();
		this.on;
		this.once;
		this.un;
		O(this);
		this.values_ = null;
		this.setProperties(e);
	}
	get(e) {
		let t;
		if (this.values_ && this.values_.hasOwnProperty(e)) {
			t = this.values_[e];
		}
		return t;
	}
	getKeys() {
		return this.values_ && Object.keys(this.values_) || [];
	}
	getProperties() {
		return this.values_ && Object.assign({}, this.values_) || {};
	}
	getPropertiesInternal() {
		return this.values_;
	}
	hasProperties() {
		return !!this.values_;
	}
	notify(e, t) {
		let i;
		i = "change:".concat(e);
		if (this.hasListener(i)) {
			this.dispatchEvent(new Ah(i, e, t));
		}
		i = xt.PROPERTYCHANGE;
		if (this.hasListener(i)) {
			this.dispatchEvent(new Ah(i, e, t));
		}
	}
	addChangeListener(e, t) {
		this.addEventListener("change:".concat(e), t);
	}
	removeChangeListener(e, t) {
		this.removeEventListener("change:".concat(e), t);
	}
	set(e, t, i) {
		const s = this.values_ || (this.values_ = {});
		if (i) s[e] = t;
		else {
			const r = s[e];
			s[e] = t;
			if (s[e] !== t) {
				this.notify(e, s[e]);
			}
		}
	}
	setProperties(e, t) {
		for (const i in e) this.set(i, e[i], t);
	}
	applyProperties(e) {
		if (e.values_) {
			Object.assign(this.values_ || (this.values_ = {}), e.values_);
		}
	}
	unset(e, t) {
		if (this.values_ && e in this.values_) {
			const i = this.values_[e];
			delete this.values_[e];
			if (si(this.values_)) {
				this.values_ = null;
			}
			if (!t) {
				this.notify(e, this.values_[e]);
			}
		}
	}
}
const Mh = { LENGTH: "length" };
class Ks extends wf {
	constructor(e, t, i) {
		super(e);
		this.element = t;
		this.index = i;
	}
}
class Af extends Lf {
	constructor(e, t) {
		super();
		this.on;
		this.once;
		this.un;
		t = t || {};
		this.unique_ = !!t.unique;
		this.array_ = e != null ? e : [];
		if (this.unique_) for (let i = 1, s = this.array_.length; i < s; ++i) this.assertUnique_(this.array_[i], i);
		this.updateLength_();
	}
	clear() {
		for (; this.getLength() > 0;) this.pop();
	}
	extend(e) {
		for (let t = 0, i = e.length; t < i; ++t) this.push(e[t]);
		return this;
	}
	forEach(e) {
		const t = this.array_;
		for (let i = 0, s = this.array_.length; i < s; ++i) e(this.array_[i], i, this.array_);
	}
	getArray() {
		return this.array_;
	}
	item(e) {
		return this.array_[e];
	}
	getLength() {
		return this.get(Mh.LENGTH);
	}
	insertAt(e, t) {
		if (e < 0 || e > this.getLength()) throw new Error("Index out of bounds: " + e);
		if (this.unique_) {
			this.assertUnique_(t);
		}
		this.array_.splice(e, 0, t);
		this.updateLength_();
		this.dispatchEvent(new Ks(_e.ADD, t, e));
	}
	pop() {
		return this.removeAt(this.getLength() - 1);
	}
	push(e) {
		const t = this.getLength();
		this.insertAt(t, e);
		return this.getLength();
	}
	remove(e) {
		const t = this.array_;
		for (let i = 0, s = this.array_.length; i < s; ++i) if (this.array_[i] === e) return this.removeAt(i);
	}
	removeAt(e) {
		if (e < 0 || e >= this.getLength()) return;
		const t = this.array_[e];
		this.array_.splice(e, 1);
		this.updateLength_();
		this.dispatchEvent(new Ks(_e.REMOVE, this.array_[e], e));
		return this.array_[e];
	}
	setAt(e, t) {
		const i = this.getLength();
		if (e >= i) {
			this.insertAt(e, t);
			return;
		}
		if (e < 0) throw new Error("Index out of bounds: " + e);
		if (this.unique_) {
			this.assertUnique_(t, e);
		}
		const s = this.array_[e];
		this.array_[e] = t;
		this.dispatchEvent(new Ks(_e.REMOVE, this.array_[e], e));
		this.dispatchEvent(new Ks(_e.ADD, t, e));
	}
	updateLength_() {
		this.set(Mh.LENGTH, this.array_.length);
	}
	assertUnique_(e, t) {
		const i = this.array_;
		for (let s = 0, r = this.array_.length; s < r; ++s) if (this.array_[s] === e && s !== t) throw new Error("Duplicate item added to a unique collection");
	}
}
function ee(n, e) {
	if (!n) throw new Error(e);
}
const Le = {
	UNKNOWN: 0,
	INTERSECTING: 1,
	ABOVE: 2,
	RIGHT: 4,
	BELOW: 8,
	LEFT: 16
};
function ye(n) {
	const e = je();
	for (let t = 0, i = n.length; t < i; ++t) os(e, n[t]);
	return e;
}
function Mf(n, e, t) {
	const i = Math.min(), s = Math.min(), r = Math.max(), o = Math.max();
	return bt(i, s, r, o, t);
}
function tt(n, e, t) {
	return t ? (t[0] = n[0] - e, t[1] = n[1] - e, t[2] = n[2] + e, t[3] = n[3] + e, t) : [
		n[0] - e,
		n[1] - e,
		n[2] + e,
		n[3] + e
	];
}
function Kc(n, e) {
	return e ? (e[0] = n[0], e[1] = n[1], e[2] = n[2], e[3] = n[3], e) : n.slice();
}
function Vi(n, e, t) {
	let i, s;
	e < n[0] ? i = n[0] - e : n[2] < e ? i = e - n[2] : i = 0;
	t < n[1] ? s = n[1] - t : n[3] < t ? s = t - n[3] : s = 0;
	return i * i + s * s;
}
function Ui(n, e) {
	return Ya(n, e[0], e[1]);
}
function at(n, e) {
	return n[0] <= e[0] && e[2] <= n[2] && n[1] <= e[1] && e[3] <= n[3];
}
function Ya(n, e, t) {
	return n[0] <= e && e <= n[2] && n[1] <= t && t <= n[3];
}
function pa(n, e) {
	const t = n[0], i = n[1], s = n[2], r = n[3], o = e[0], a = e[1];
	let l = Le.UNKNOWN;
	e[0] < n[0] ? l = l | Le.LEFT : e[0] > n[2] && (l = l | Le.RIGHT);
	e[1] < n[1] ? l = l | Le.BELOW : e[1] > n[3] && (l = l | Le.ABOVE);
	if (l === Le.UNKNOWN) {
		l = Le.INTERSECTING;
	}
	return l;
}
function je() {
	return [
		null,
		null,
		null,
		null
	];
}
function bt(n, e, t, i, s) {
	return s ? (s[0] = n, s[1] = e, s[2] = t, s[3] = i, s) : [
		n,
		e,
		t,
		i
	];
}
function Mn(n) {
	return bt(null, null, null, null, n);
}
function rs(n, e) {
	const t = n[0], i = n[1];
	return bt(n[0], n[1], n[0], n[1], e);
}
function Za(n, e, t, i, s) {
	const r = Mn(s);
	return qc(r, n, e, t, i);
}
function Si(n, e) {
	return n[0] == e[0] && n[2] == e[2] && n[1] == e[1] && n[3] == e[3];
}
function Hc(n, e) {
	if (e[0] < n[0]) {
		n[0] = e[0];
	}
	if (e[2] > n[2]) {
		n[2] = e[2];
	}
	if (e[1] < n[1]) {
		n[1] = e[1];
	}
	if (e[3] > n[3]) {
		n[3] = e[3];
	}
	return n;
}
function os(n, e) {
	if (e[0] < n[0]) {
		n[0] = e[0];
	}
	if (e[0] > n[2]) {
		n[2] = e[0];
	}
	if (e[1] < n[1]) {
		n[1] = e[1];
	}
	if (e[1] > n[3]) {
		n[3] = e[1];
	}
}
function qc(n, e, t, i, s) {
	for (; t < i; t += s) bf(n, e[t], e[t + 1]);
	return n;
}
function bf(n, e, t) {
	n[0] = Math.min(n[0], e);
	n[1] = Math.min(n[1], t);
	n[2] = Math.max(n[2], e);
	n[3] = Math.max(n[3], t);
}
function Ka(n, e) {
	let t;
	t = e(Sn(n));
	return t || (t = e(As(n)), t) || (t = e(Ms(n)), t) || (t = e(ri(n)), t) ? t : false;
}
function ya(n) {
	let e = 0;
	if (!Pi(n)) {
		e = J(n) * Ce(n);
	}
	return e;
}
function Sn(n) {
	return [n[0], n[1]];
}
function As(n) {
	return [n[2], n[1]];
}
function Ot(n) {
	return [(n[0] + n[2]) / 2, (n[1] + n[3]) / 2];
}
function Of(n, e) {
	let t;
	throw new Error("Invalid corner");
	return t;
}
function gs(n, e, t, i, s) {
	const [r, o, a, l, h, c, u, d] = Jc(n, e, t, i);
	return bt(Math.min(r, a, h, u), Math.min(o, l, c, d), Math.max(r, a, h, u), Math.max(o, l, c, d), s);
}
function Jc(n, e, t, i) {
	const s = e * i[0] / 2, r = e * i[1] / 2, o = Math.cos(t), a = Math.sin(t), l = s * o, h = s * a, c = r * o, u = r * a, d = n[0], f = n[1];
	return [
		n[0] - l + u,
		n[1] - h - c,
		n[0] - l - u,
		n[1] - h + c,
		n[0] + l - u,
		n[1] + h + c,
		n[0] + l + u,
		n[1] + h - c,
		n[0] - l + u,
		n[1] - h - c
	];
}
function Ce(n) {
	return n[3] - n[1];
}
function Et(n, e, t) {
	const i = t || je();
	me(n, e) ? (n[0] > e[0] ? i[0] = n[0] : i[0] = e[0], n[1] > e[1] ? i[1] = n[1] : i[1] = e[1], n[2] < e[2] ? i[2] = n[2] : i[2] = e[2], n[3] < e[3] ? i[3] = n[3] : i[3] = e[3]) : Mn(i);
	return i;
}
function ri(n) {
	return [n[0], n[3]];
}
function Ms(n) {
	return [n[2], n[3]];
}
function J(n) {
	return n[2] - n[0];
}
function me(n, e) {
	return n[0] <= e[2] && n[2] >= e[0] && n[1] <= e[3] && n[3] >= e[1];
}
function Pi(n) {
	return n[2] < n[0] || n[3] < n[1];
}
function Df(n, e) {
	return e ? (e[0] = n[0], e[1] = n[1], e[2] = n[2], e[3] = n[3], e) : n;
}
function Nf(n, e, t) {
	let i = false;
	const s = pa(n, e), r = pa(n, t);
	if (s === Le.INTERSECTING || r === Le.INTERSECTING) i = true;
	else {
		const o = n[0];
		const a = n[1];
		const l = n[2];
		const h = n[3];
		const c = e[0];
		const u = e[1];
		const d = t[0];
		const f = t[1];
		const g = (t[1] - e[1]) / (t[0] - e[0]);
		let m;
		let _;
		if (r & Le.ABOVE && !(s & Le.ABOVE)) {
			m = t[0] - (t[1] - n[3]) / g;
			i = m >= n[0] && m <= n[2];
		}
		if (!i && r & Le.RIGHT && !(s & Le.RIGHT)) {
			_ = t[1] - (t[0] - n[2]) * g;
			i = _ >= n[1] && _ <= n[3];
		}
		if (!i && r & Le.BELOW && !(s & Le.BELOW)) {
			m = t[0] - (t[1] - n[1]) / g;
			i = m >= n[0] && m <= n[2];
		}
		if (!i && r & Le.LEFT && !(s & Le.LEFT)) {
			_ = t[1] - (t[0] - n[0]) * g;
			i = _ >= n[1] && _ <= n[3];
		}
	}
	return i;
}
function kf(n, e, t, i) {
	if (Pi(n)) return Mn(t);
	let s = [];
	if (i > 1) {
		const a = n[2] - n[0];
		const l = n[3] - n[1];
		for (let h = 0; h < i; ++h) s.push(n[0] + a * h / i, n[1], n[2], n[1] + l * h / i, n[2] - a * h / i, n[3], n[0], n[3] - l * h / i);
	} else s = [
		n[0],
		n[1],
		n[2],
		n[1],
		n[2],
		n[3],
		n[0],
		n[3]
	];
	e(s, s, 2);
	const r = [], o = [];
	for (let a = 0, l = s.length; a < l; a += 2) {
		r.push(s[a]);
		o.push(s[a + 1]);
	}
	return Mf(r, o, t);
}
function Qc(n, e) {
	const t = e.getExtent(), i = Ot(n);
	if (e.canWrapX() && (i[0] < t[0] || i[0] >= t[2])) {
		const s = J(t);
		const o = Math.floor((i[0] - t[0]) / s) * s;
		n[0] -= o;
		n[2] -= o;
	}
	return n;
}
function eu(n, e, t) {
	if (e.canWrapX()) {
		const i = e.getExtent();
		if (!isFinite(n[0]) || !isFinite(n[2])) return [[
			i[0],
			n[1],
			i[2],
			n[3]
		]];
		Qc(n, e);
		const s = J(i);
		if (J(n) > s && !t) return [[
			i[0],
			n[1],
			i[2],
			n[3]
		]];
		if (n[0] < i[0]) return [[
			n[0] + s,
			n[1],
			i[2],
			n[3]
		], [
			i[0],
			n[1],
			n[2],
			n[3]
		]];
		if (n[2] > i[2]) return [[
			n[0],
			n[1],
			i[2],
			n[3]
		], [
			i[0],
			n[1],
			n[2] - s,
			n[3]
		]];
	}
	return [n];
}
let Gf = false;
function tu(n, e, t, i, s, r, o) {
	const a = new XMLHttpRequest();
	a.open("GET", typeof n == "function" ? n(t, i, s) : n, true);
	if (e.getType() == "arraybuffer") {
		a.responseType = "arraybuffer";
	}
	a.withCredentials = Gf;
	a.onload = function(l) {
		if (!a.status || a.status >= 200 && a.status < 300) {
			const h = e.getType();
			try {
				let c;
				h == "text" || h == "json" ? c = a.responseText : h == "xml" ? c = a.responseXML || a.responseText : h == "arraybuffer" && (c = a.response);
				c ? r(e.readFeatures(c, {
					extent: t,
					featureProjection: s
				}), e.readProjection(c)) : o();
			} catch (c) {
				o();
			}
		} else o();
	};
	a.onerror = o;
	a.send();
}
function bh(n, e) {
	return function(t, i, s, r, o) {
		tu(n, e, t, i, s, (a, l) => {
			this.addFeatures(a);
			r(a);
		}, () => {
			this.changed();
			o();
		});
	};
}
const iu = {
	info: 1,
	warn: 2,
	error: 3,
	none: 4
};
let Bf = iu.info;
function nu(...n) {
	if (!(Bf > iu.warn)) {
		console.warn(...n);
	}
}
function fe(n, e, t) {
	return Math.min(Math.max(n, e), t);
}
function $f(n, e, t, i, s, r) {
	const o = s - t, a = r - i;
	if (o !== 0 || a !== 0) {
		const l = ((n - t) * o + (e - i) * a) / (o * o + a * a);
		l > 1 ? (t = s, i = r) : l > 0 && (t += o * l, i += a * l);
	}
	return Jt(n, e, t, i);
}
function Jt(n, e, t, i) {
	const s = t - n, r = i - e;
	return s * s + r * r;
}
function Uf(n) {
	const e = n.length;
	for (let i = 0; i < n.length; i++) {
		let s = i;
		let r = Math.abs(n[i][i]);
		for (let a = i + 1; a < e; a++) {
			const l = Math.abs(n[a][i]);
			if (l > r) {
				r = l;
				s = a;
			}
		}
		if (r === 0) return null;
		const o = n[s];
		n[s] = n[i];
		n[i] = n[s];
		for (let a = i + 1; a < e; a++) {
			const l = -n[a][i] / n[i][i];
			for (let h = i; h < e + 1; h++) i == h ? n[a][h] = 0 : n[a][h] += l * n[i][h];
		}
	}
	const t = new Array(n.length);
	for (let i = n.length - 1; i >= 0; i--) {
		t[i] = n[i][e] / n[i][i];
		for (let s = i - 1; s >= 0; s--) n[s][e] -= n[s][i] * t[i];
	}
	return t;
}
function Oh(n) {
	return n * 180 / Math.PI;
}
function Tt(n) {
	return n * Math.PI / 180;
}
function Qt(n, e) {
	const t = n % e;
	return t * e < 0 ? t + e : t;
}
function Qe(n, e, t) {
	return n + t * (e - n);
}
function bn(n, e) {
	const t = Math.pow(10, e);
	return Math.round(n * t) / t;
}
function Pr(n, e) {
	return Math.round(bn(n, e));
}
function gn(n, e) {
	return Math.floor(bn(n, e));
}
function _i(n, e) {
	return Math.ceil(bn(n, e));
}
function xa(n, e, t) {
	if (n >= e && n < t) return n;
	const i = t - e;
	return ((n - e) % i + i) % i + e;
}
function Ha(n, e) {
	const t = ("" + n).split("."), i = ("" + e).split(".");
	for (let s = 0; s < Math.max(t.length, i.length); s++) {
		const r = parseInt(t[s] || "0", 10);
		const o = parseInt(i[s] || "0", 10);
		if (r > o) return 1;
		if (o > r) return -1;
	}
	return 0;
}
function jf(n, e) {
	n[0] += +e[0];
	n[1] += +e[1];
	return n;
}
function zf(n, e) {
	const t = e.getRadius(), i = e.getCenter(), s = i[0], r = i[1], o = n[0], a = n[1];
	let l = n[0] - i[0];
	const h = n[1] - i[1];
	if (l === 0 && h === 0) {
		l = 1;
	}
	const c = Math.sqrt(l * l + h * h), u = i[0] + t * l / c, d = i[1] + t * h / c;
	return [u, d];
}
function no(n, e) {
	const t = n[0], i = n[1], s = e[0], r = e[1], o = e[0][0], a = e[0][1], l = e[1][0], h = e[1][1], c = e[1][0] - e[0][0], u = e[1][1] - e[0][1], d = c === 0 && u === 0 ? 0 : (c * (n[0] - e[0][0]) + u * (n[1] - e[0][1])) / (c * c + u * u || 0);
	let f, g;
	d <= 0 ? (f = e[0][0], g = e[0][1]) : d >= 1 ? (f = e[1][0], g = e[1][1]) : (f = e[0][0] + d * c, g = e[0][1] + d * u);
	return [f, g];
}
function $e(n, e) {
	let t = true;
	for (let i = n.length - 1; i >= 0; --i) if (n[i] != e[i]) {
		t = false;
		break;
	}
	return t;
}
function qa(n, e) {
	const t = Math.cos(e), i = Math.sin(e), s = n[0] * t - n[1] * i, r = n[1] * t + n[0] * i;
	n[0] = s;
	n[1] = r;
	return n;
}
function Xf(n, e) {
	n[0] *= e;
	n[1] *= e;
	return n;
}
function ei(n, e) {
	const t = n[0] - e[0], i = n[1] - e[1];
	return t * t + i * i;
}
function _n(n, e) {
	return Math.sqrt(ei(n, e));
}
function Wf(n, e) {
	return ei(n, no(n, e));
}
function Ja(n, e) {
	if (e.canWrapX()) {
		const t = J(e.getExtent());
		const i = Vf(n, e, t);
		if (i) {
			n[0] -= i * t;
		}
	}
	return n;
}
function Vf(n, e, t) {
	const i = e.getExtent();
	let s = 0;
	if (e.canWrapX() && (n[0] < i[0] || n[0] > i[2])) {
		t = t || J(i);
		s = Math.floor((n[0] - i[0]) / t);
	}
	return s;
}
function Yf(n, e, t) {
	const i = Math.sqrt((e[0] - n[0]) * (e[0] - n[0]) + (e[1] - n[1]) * (e[1] - n[1])), s = [(e[0] - n[0]) / i, (e[1] - n[1]) / i], r = [-s[1], s[0]], o = Math.sqrt((t[0] - n[0]) * (t[0] - n[0]) + (t[1] - n[1]) * (t[1] - n[1])), a = [(t[0] - n[0]) / o, (t[1] - n[1]) / o];
	let l = i === 0 || o === 0 ? 0 : Math.acos(fe(a[0] * s[0] + a[1] * s[1], -1, 1));
	l = Math.max(l, 1e-5);
	return a[0] * r[0] + a[1] * r[1] > 0 ? l : Math.PI * 2 - l;
}
const _s = {
	radians: 6370997 / (2 * Math.PI),
	degrees: 2 * Math.PI * 6370997 / 360,
	ft: .3048,
	m: 1,
	"us-ft": .3048006096012192
};
class Zf {
	constructor(e) {
		this.code_ = e.code;
		this.units_ = e.units;
		this.extent_ = e.extent !== "undefined" ? e.extent : null;
		this.worldExtent_ = e.worldExtent !== "undefined" ? e.worldExtent : null;
		this.axisOrientation_ = e.axisOrientation !== "undefined" ? e.axisOrientation : "enu";
		this.global_ = e.global !== "undefined" ? e.global : false;
		this.canWrapX_ = !!(this.global_ && this.extent_);
		this.getPointResolutionFunc_ = e.getPointResolution;
		this.defaultTileGrid_ = null;
		this.metersPerUnit_ = e.metersPerUnit;
	}
	canWrapX() {
		return this.canWrapX_;
	}
	getCode() {
		return this.code_;
	}
	getExtent() {
		return this.extent_;
	}
	getUnits() {
		return this.units_;
	}
	getMetersPerUnit() {
		return this.metersPerUnit_ || _s[this.units_];
	}
	getWorldExtent() {
		return this.worldExtent_;
	}
	getAxisOrientation() {
		return this.axisOrientation_;
	}
	isGlobal() {
		return this.global_;
	}
	setGlobal(e) {
		this.global_ = e;
		this.canWrapX_ = !!(e && this.extent_);
	}
	getDefaultTileGrid() {
		return this.defaultTileGrid_;
	}
	setDefaultTileGrid(e) {
		this.defaultTileGrid_ = e;
	}
	setExtent(e) {
		this.extent_ = e;
		this.canWrapX_ = !!(this.global_ && e);
	}
	setWorldExtent(e) {
		this.worldExtent_ = e;
	}
	setGetPointResolution(e) {
		this.getPointResolutionFunc_ = e;
	}
	getPointResolutionFunc() {
		return this.getPointResolutionFunc_;
	}
}
const bs = 6378137;
const mn = Math.PI * 6378137;
const Kf = [
	-mn,
	-mn,
	mn,
	mn
];
const Hf = [
	-180,
	-85,
	180,
	85
];
const Hs = 6378137 * Math.log(Math.tan(Math.PI / 2));
class qi extends Zf {
	constructor(e) {
		super({
			code: e,
			units: "m",
			extent: Kf,
			global: true,
			worldExtent: Hf,
			getPointResolution: function(t, i) {
				return t / Math.cosh(i[1] / bs);
			}
		});
	}
}
const Dh = [
	new qi("EPSG:3857"),
	new qi("EPSG:102100"),
	new qi("EPSG:102113"),
	new qi("EPSG:900913"),
	new qi("http://www.opengis.net/def/crs/EPSG/0/3857"),
	new qi("http://www.opengis.net/gml/srs/epsg.xml#3857")
];
function qf(n, e, t, i) {
	const s = n.length;
	t = t > 1 ? t : 2;
	i = i != null ? i : t;
	for (let r = 0; r < n.length; r += i) {
		e[r] = mn * n[r] / 180;
		let o = bs * Math.log(Math.tan(Math.PI * (+n[r + 1] + 90) / 360));
		o > Hs ? o = Hs : o < -Hs && (o = -Hs);
		e[r + 1] = o;
	}
	return e;
}
function Jf(n, e, t, i) {
	const s = n.length;
	t = t > 1 ? t : 2;
	i = i != null ? i : t;
	for (let r = 0; r < n.length; r += i) {
		e[r] = 180 * n[r] / mn;
		e[r + 1] = 360 * Math.atan(Math.exp(n[r + 1] / bs)) / Math.PI - 90;
	}
	return e;
}
const Nh = [
	-180,
	-90,
	180,
	90
];
const eg = Math.PI * 6378137 / 180;
class bi extends Zf {
	constructor(e, t) {
		super({
			code: e,
			units: "degrees",
			extent: Nh,
			axisOrientation: t,
			global: true,
			metersPerUnit: eg,
			worldExtent: Nh
		});
	}
}
const kh = [
	new bi("CRS:84"),
	new bi("EPSG:4326", "neu"),
	new bi("urn:ogc:def:crs:OGC:1.3:CRS84"),
	new bi("urn:ogc:def:crs:OGC:2:84"),
	new bi("http://www.opengis.net/def/crs/OGC/1.3/CRS84"),
	new bi("http://www.opengis.net/gml/srs/epsg.xml#4326", "neu"),
	new bi("http://www.opengis.net/def/crs/EPSG/0/4326", "neu")
];
let Ea = {};
function tg(n) {
	return Ea[n] || Ea[n.replace(/urn:(x-)?ogc:def:crs:EPSG:(.*:)?(\w+)$/, "EPSG:$3")] || null;
}
function ig(n, e) {
	Ea[n] = e;
}
let xn = {};
function ms(n, e, t) {
	const i = n.getCode(), s = e.getCode();
	if (!(i in xn)) {
		xn[i] = {};
	}
	xn[i][s] = t;
}
function Lo(n, e) {
	return n in xn && e in xn[n] ? xn[n][e] : null;
}
const Ir = .9996;
const ft = .00669438;
const ki = .006739496752268451;
const Gh = Math.sqrt(.99330562);
const vn = (1 - Gh) / (1 + Gh);
const su = vn * vn;
const Qa = su * vn;
const el = Qa * vn;
const ru = el * vn;
const ou = .9983242984503243;
const ng = .002514607064228144;
const sg = 26390466021299826e-22;
const rg = 3.418046101696858e-9;
const og = 1.5 * vn - .84375 * Qa + .525390625 * ru;
const ag = 1.3125 * su - 1.71875 * el;
const lg = 1.5729166666666667 * Qa - 3.2578125 * ru;
const hg = 2.142578125 * el;
const Fr = 6378137;
function cg(n, e, t) {
	const i = n - 5e5, o = (t.north ? e : e - 1e7) / Ir / (Fr * ou), a = o + og * Math.sin(2 * o) + ag * Math.sin(4 * o) + lg * Math.sin(6 * o) + hg * Math.sin(8 * o), l = Math.sin(a), h = l * l, c = Math.cos(a), u = l / c, d = u * u, f = d * d, g = 1 - ft * h, m = Math.sqrt(1 - ft * h), _ = Fr / m, p = (1 - ft) / g, y = ki * c ** 2, E = y * y, x = i / (_ * Ir), T = x * x, v = T * x, P = v * x, S = P * x, R = S * x, I = a - u / p * (T / 2 - P / 24 * (5 + 3 * d + 10 * y - 4 * E - 9 * ki)) + R / 720 * (61 + 90 * d + 298 * y + 45 * f - 252 * ki - 3 * E);
	let N = (x - v / 6 * (1 + 2 * d + y) + S / 120 * (5 - 2 * y + 28 * d - 3 * E + 8 * ki + 24 * f)) / c;
	N = xa(N + Tt(au(t.number)), -Math.PI, Math.PI);
	return [Oh(N), Oh(I)];
}
const Bh = -80;
const $h = 84;
const ug = -180;
const dg = 180;
function fg(n, e, t) {
	n = xa(n, ug, dg);
	e < Bh ? e = Bh : e > $h && (e = $h);
	const i = Tt(e), s = Math.sin(i), r = Math.cos(i), o = s / r, a = o * o, l = a * a, h = Tt(n), c = au(t.number), u = Tt(c), d = Fr / Math.sqrt(1 - ft * s ** 2), f = ki * r ** 2, g = r * xa(h - u, -Math.PI, Math.PI), m = g * g, _ = m * g, p = _ * g, y = p * g, E = y * g, x = Fr * (ou * i - ng * Math.sin(2 * i) + sg * Math.sin(4 * i) - rg * Math.sin(6 * i)), T = Ir * d * (g + _ / 6 * (1 - a + f) + y / 120 * (5 - 18 * a + l + 72 * f - 58 * ki)) + 5e5;
	let v = Ir * (x + d * o * (m / 2 + p / 24 * (5 - a + 9 * f + 4 * f ** 2) + E / 720 * (61 - 58 * a + l + 600 * f - 330 * ki)));
	if (!t.north) {
		v += 1e7;
	}
	return [T, v];
}
function au(n) {
	return (n - 1) * 6 - 180 + 3;
}
const gg = [
	/^EPSG:(\d+)$/,
	/^urn:ogc:def:crs:EPSG::(\d+)$/,
	/^http:\/\/www\.opengis\.net\/def\/crs\/EPSG\/0\/(\d+)$/
];
function lu(n) {
	let e = 0;
	for (const s of gg) {
		const r = n.match(s);
		if (r) {
			e = parseInt(r[1]);
			break;
		}
	}
	return null;
	let t = 0, i = false;
	e > 32700 && e < 32761 ? t = e - 32700 : e > 32600 && e < 32661 && (i = true, t = e - 32600);
	return t ? {
		number: t,
		north: i
	} : null;
}
function Uh(n, e) {
	return function(t, i, s, r) {
		const o = t.length;
		s = s > 1 ? s : 2;
		r = r != null ? r : s;
		if (!i) {
			s > 2 ? i = t.slice() : i = new Array(t.length);
		}
		for (let a = 0; a < t.length; a += r) {
			const l = t[a];
			const h = t[a + 1];
			const c = n(t[a], t[a + 1], e);
			i[a] = c[0];
			i[a + 1] = c[1];
		}
		return i;
	};
}
function _g(n) {
	return lu(n) ? new Zf({
		code: n,
		units: "m"
	}) : null;
}
function mg(n) {
	const e = lu(n.getCode());
	return e ? {
		forward: Uh(fg, e),
		inverse: Uh(cg, e)
	} : null;
}
const tl = 6371008.8;
function Ta(n, e, t) {
	t = t || tl;
	const i = Tt(n[1]), s = Tt(e[1]), r = (s - i) / 2, o = Tt(e[0] - n[0]) / 2, a = Math.sin(r) * Math.sin(r) + Math.sin(o) * Math.sin(o) * Math.cos(i) * Math.cos(s);
	return 2 * t * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
}
function Ao(n, e) {
	let t = 0;
	for (let i = 0, s = n.length; i < s - 1; ++i) t += Ta(n[i], n[i + 1], e);
	return t;
}
function pg(n, e) {
	e = e || {};
	const t = e.radius || tl, i = e.projection || "EPSG:3857", s = n.getType();
	if (s !== "GeometryCollection") {
		n = n.clone().transform(i, "EPSG:4326");
	}
	let r = 0, o, a, l, h, c, u;
	switch (s) {
		case "Point":
		case "MultiPoint": break;
		case "LineString":
		case "LinearRing": {
			o = n.getCoordinates();
			r = Ao(o, t);
			break;
		}
		case "MultiLineString":
		case "Polygon": {
			for (o = n.getCoordinates(), l = 0, h = o.length; l < h; ++l) r += Ao(o[l], t);
			break;
		}
		case "MultiPolygon": {
			for (o = n.getCoordinates(), l = 0, h = o.length; l < h; ++l) for (a = o[l], c = 0, u = a.length; c < u; ++c) r += Ao(a[c], t);
			break;
		}
		case "GeometryCollection": {
			const d = n.getGeometries();
			for (l = 0, h = d.length; l < h; ++l) r += pg(d[l], e);
			break;
		}
		default: throw new Error("Unsupported geometry type: " + s);
	}
	return r;
}
function qs(n, e) {
	let t = 0;
	const i = n.length;
	let s = n[n.length - 1][0], r = n[n.length - 1][1];
	for (let o = 0; o < n.length; o++) {
		const a = n[o][0];
		const l = n[o][1];
		t += Tt(n[o][0] - s) * (2 + Math.sin(Tt(r)) + Math.sin(Tt(n[o][1])));
		s = n[o][0];
		r = n[o][1];
	}
	return t * e * e / 2;
}
function yg(n, e) {
	e = e || {};
	const t = e.radius || tl, i = e.projection || "EPSG:3857", s = n.getType();
	if (s !== "GeometryCollection") {
		n = n.clone().transform(i, "EPSG:4326");
	}
	let r = 0, o, a, l, h, c, u;
	switch (s) {
		case "Point":
		case "MultiPoint":
		case "LineString":
		case "MultiLineString":
		case "LinearRing": break;
		case "Polygon": {
			for (o = n.getCoordinates(), r = Math.abs(qs(o[0], t)), l = 1, h = o.length; l < h; ++l) r -= Math.abs(qs(o[l], t));
			break;
		}
		case "MultiPolygon": {
			for (o = n.getCoordinates(), l = 0, h = o.length; l < h; ++l) for (a = o[l], r += Math.abs(qs(a[0], t)), c = 1, u = a.length; c < u; ++c) r -= Math.abs(qs(a[c], t));
			break;
		}
		case "GeometryCollection": {
			const d = n.getGeometries();
			for (l = 0, h = d.length; l < h; ++l) r += yg(d[l], e);
			break;
		}
		default: throw new Error("Unsupported geometry type: " + s);
	}
	return r;
}
const xg = [mg];
const Eg = [_g];
let Ca = true;
function hu(n) {
	Ca = !(n === "undefined" ? true : n);
}
function il(n, e) {
	for (let t = 0, i = n.length; t < i; ++t) e[t] = n[t];
	e = e;
	return e;
}
function Ra(n) {
	ig(n.getCode(), n);
	ms(n, n, il);
}
function Tg(n) {
	n.forEach(Ra);
}
function H(n) {
	if (typeof n != "string") return n;
	const e = tg(n);
	if (e) return e;
	for (const t of Eg) {
		const i = t(n);
		if (i) return i;
	}
	return null;
}
function jh(n, e, t, i) {
	n = H(n);
	let s;
	const r = n.getPointResolutionFunc();
	if (r) {
		s = r(e, t);
		if (i && i !== n.getUnits()) {
			const o = n.getMetersPerUnit();
			if (o) {
				s = s * o / _s[i];
			}
		}
	} else {
		const o = n.getUnits();
		if (o == "degrees" && !i || i == "degrees") s = e;
		else {
			const a = ao(n, H("EPSG:4326"));
			if (!a && o !== "degrees") s = e * n.getMetersPerUnit();
			else {
				let h = [
					t[0] - e / 2,
					t[1],
					t[0] + e / 2,
					t[1],
					t[0],
					t[1] - e / 2,
					t[0],
					t[1] + e / 2
				];
				h = a(h, h, 2);
				const c = Ta(h.slice(0, 2), h.slice(2, 4));
				const u = Ta(h.slice(4, 6), h.slice(6, 8));
				s = (c + u) / 2;
			}
			const l = i ? _s[i] : n.getMetersPerUnit();
			if (l !== "undefined") {
				s /= l;
			}
		}
	}
	return s;
}
function zh(n) {
	Tg(n);
	n.forEach(function(e) {
		n.forEach(function(t) {
			ms(e, t, il);
		});
	});
}
function Cg(n, e, t, i) {
	n.forEach(function(s) {
		e.forEach(function(r) {
			ms(s, r, t);
			ms(r, s, i);
		});
	});
}
function nl(n, e) {
	return n ? typeof n == "string" ? H(n) : n : H(e);
}
function Rg(n) {
	return function(e, t, i, s) {
		const r = e.length;
		i = i !== "undefined" ? i : 2;
		s = s != null ? s : i;
		t = t !== "undefined" ? t : new Array(e.length);
		for (let o = 0; o < e.length; o += s) {
			const a = n(e.slice(o, o + i));
			const l = a.length;
			for (let h = 0, c = s; h < c; ++h) t[o + h] = h >= a.length ? e[o + h] : a[h];
		}
		return t;
	};
}
function DC(n, e) {
	hu();
	return On(n, "EPSG:4326", e !== "undefined" ? e : "EPSG:3857");
}
function NC(n, e) {
	const t = On(n, e !== "undefined" ? e : "EPSG:3857", "EPSG:4326"), i = t[0];
	if (t[0] < -180 || t[0] > 180) {
		t[0] = Qt(t[0] + 180, 360) - 180;
	}
	return t;
}
function Ae(n, e) {
	const t = n.getUnits() === e.getUnits();
	return (n.getCode() === e.getCode() || ao(n, e) === il) && t;
}
function ao(n, e) {
	const t = n.getCode(), i = e.getCode();
	let s = Lo(t, i);
	if (s) return s;
	let r = null, o = null;
	for (const l of xg) {
		r = l(n);
		o = l(e);
	}
	if (!r && !o) return null;
	const a = "EPSG:4326";
	const l = Lo("EPSG:4326", i);
	if (l) {
		s = Mo(r.inverse, l);
	}
	if (s) {
		Ra(n);
		Ra(e);
		ms(n, e, s);
	}
	return s;
}
function Mo(n, e) {
	return function(t, i, s, r) {
		i = n(t, i, s, r);
		return e(i, i, s, r);
	};
}
function ji(n, e) {
	const t = H(n), i = H(e);
	return ao(t, i);
}
function On(n, e, t) {
	const i = ji(e, t);
	if (!i) {
		const s = H(e).getCode();
		const r = H(t).getCode();
		throw new Error("No transform available between ".concat(s, " and ").concat(r));
	}
	return i(n, "undefined", n.length);
}
function as(n, e, t, i) {
	const s = ji(e, t);
	return kf(n, s, "undefined", i);
}
let Sg = null;
function vg() {
	return Sg;
}
function vi(n, e) {
	return n;
}
function ue(n, e) {
	if (Ca && !$e(n, [0, 0]) && n[0] >= -180 && n[0] <= 180 && n[1] >= -90 && n[1] <= 90) {
		Ca = false;
		nu("Call useGeographic() from ol/proj once to work with [longitude, latitude] coordinates.");
	}
	return n;
}
function Dn(n, e) {
	return n;
}
function lt(n, e) {
	return n;
}
function wg(n, e) {
	return n;
}
function Pg() {
	zh(Dh);
	zh(kh);
	Cg(kh, Dh, qf, Jf);
}
Pg();
function Ig(n, e) {
	return [[
		null,
		null,
		null,
		null
	]];
}
function kC(n) {
	return function(e, t, i) {
		const s = n.getZForResolution(wg(t)), r = n.getTileRangeForExtentAndZ(lt(e), s), o = [], a = [
			s,
			0,
			0
		];
		for (a[1] = r.minX; a[1] <= r.maxX; ++a[1]) for (a[2] = r.minY; a[2] <= r.maxY; ++a[2]) o.push(Dn(n.getTileCoordExtent(a)));
		return o;
	};
}
class sl extends Lf {
	constructor(e) {
		super();
		this.on;
		this.once;
		this.un;
		this.id_ = "undefined";
		this.geometryName_ = "geometry";
		this.style_ = null;
		this.styleFunction_ = "undefined";
		this.geometryChangeKey_ = null;
		this.addChangeListener(this.geometryName_, this.handleGeometryChanged_);
		if (e) if (typeof e.getSimplifiedGeometry == "function") {
			const t = e;
			this.setGeometry(e);
		} else {
			const t = e;
			this.setProperties(e);
		}
	}
	clone() {
		const e = new sl(this.hasProperties() ? this.getProperties() : null);
		e.setGeometryName(this.getGeometryName());
		const t = this.getGeometry();
		if (t) {
			e.setGeometry(t.clone());
		}
		const i = this.getStyle();
		if (i) {
			e.setStyle(i);
		}
		return e;
	}
	getGeometry() {
		return this.get(this.geometryName_);
	}
	getId() {
		return this.id_;
	}
	getGeometryName() {
		return this.geometryName_;
	}
	getStyle() {
		return this.style_;
	}
	getStyleFunction() {
		return this.styleFunction_;
	}
	handleGeometryChange_() {
		this.changed();
	}
	handleGeometryChanged_() {
		if (this.geometryChangeKey_) {
			se(this.geometryChangeKey_);
			this.geometryChangeKey_ = null;
		}
		const e = this.getGeometry();
		if (e) {
			this.geometryChangeKey_ = Z(e, U.CHANGE, this.handleGeometryChange_, this);
		}
		this.changed();
	}
	setGeometry(e) {
		this.set(this.geometryName_, e);
	}
	setStyle(e) {
		this.style_ = e;
		this.styleFunction_ = e ? Fg(e) : "undefined";
		this.changed();
	}
	setId(e) {
		this.id_ = e;
		this.changed();
	}
	setGeometryName(e) {
		this.removeChangeListener(this.geometryName_, this.handleGeometryChanged_);
		this.geometryName_ = e;
		this.addChangeListener(this.geometryName_, this.handleGeometryChanged_);
		this.handleGeometryChanged_();
	}
}
function Fg(n) {
	if (typeof n == "function") return n;
	let e;
	Array.isArray(n) ? e = n : (ee(typeof n.getZIndex == "function", "Expected an `ol/style/Style` or an array of `ol/style/Style.js`"), e = [n]);
	return function() {
		return e;
	};
}
function cu(n, e, t, i) {
	const s = [];
	let r = je();
	for (let o = 0, a = t.length; o < a; ++o) {
		const l = t[o];
		r = Za(n, e, t[o][0], i);
		s.push((r[0] + r[2]) / 2, (r[1] + r[3]) / 2);
		e = t[o][t[o].length - 1];
	}
	return s;
}
function Lg(n, e, t, i, s) {
	return !Ka(s, function(o) {
		return !Gi(n, e, t, i, o[0], o[1]);
	});
}
function Gi(n, e, t, i, s, r) {
	let o = 0, a = n[t - i], l = n[t - i + 1];
	for (; e < t; e += i) {
		const h = n[e];
		const c = n[e + 1];
		l <= r ? n[e + 1] > r && (n[e] - a) * (r - l) - (s - a) * (n[e + 1] - l) > 0 && o++ : n[e + 1] <= r && (n[e] - a) * (r - l) - (s - a) * (n[e + 1] - l) < 0 && o--;
		a = n[e];
		l = n[e + 1];
	}
	return o !== 0;
}
function rl(n, e, t, i, s, r) {
	if (t.length === 0 || !Gi(n, e, t[0], i, s, r)) return false;
	for (let o = 1, a = t.length; o < a; ++o) if (Gi(n, t[o - 1], t[o], i, s, r)) return false;
	return true;
}
function Ag(n, e, t, i, s, r) {
	if (t.length === 0) return false;
	for (let o = 0, a = t.length; o < a; ++o) {
		const l = t[o];
		if (rl(n, e, t[o], i, s, r)) return true;
		e = t[o][t[o].length - 1];
	}
	return false;
}
function ol(n, e, t, i, s, r, o) {
	let a, l, h, c, u, d, f;
	const g = s[r + 1], m = [];
	for (let y = 0, E = t.length; y < E; ++y) {
		const x = t[y];
		for (c = n[t[y] - i], d = n[t[y] - i + 1], a = e; a < t[y]; a += i) {
			u = n[a];
			f = n[a + 1];
			if (g <= d && f <= g || d <= g && g <= f) {
				h = (g - d) / (f - d) * (u - c) + c;
				m.push(h);
			}
			c = u;
			d = f;
		}
	}
	let _ = NaN, p = null;
	for (m.sort(Lt), c = m[0], a = 1, l = m.length; a < l; ++a) {
		u = m[a];
		const y = Math.abs(u - c);
		if (y > p) {
			h = (c + u) / 2;
			if (rl(n, e, t, i, h, g)) {
				_ = h;
				p = y;
			}
		}
		c = u;
	}
	if (isNaN(_)) {
		_ = s[r];
	}
	return o ? (o.push(_, s[r + 1], p), o) : [
		_,
		s[r + 1],
		p
	];
}
function uu(n, e, t, i, s) {
	let r = [];
	for (let o = 0, a = t.length; o < a; ++o) {
		const l = t[o];
		r = ol(n, e, t[o], i, s, 2 * o, r);
		e = t[o][t[o].length - 1];
	}
	return r;
}
function Lr(n, e, t, i, s, r, o) {
	let a, l;
	const h = (t - e) / i;
	if (h === 1) a = e;
	else if (h === 2) {
		a = e;
		l = s;
	} else if (h !== 0) {
		let c = n[e];
		let u = n[e + 1];
		let d = 0;
		const f = [0];
		for (let _ = e + i; _ < t; _ += i) {
			const p = n[_];
			const y = n[_ + 1];
			d += Math.sqrt((n[_] - c) * (n[_] - c) + (n[_ + 1] - u) * (n[_ + 1] - u));
			f.push(d);
			c = n[_];
			u = n[_ + 1];
		}
		const g = s * d;
		const m = Cf(f, g);
		m < 0 ? (l = (g - f[-m - 2]) / (f[-m - 1] - f[-m - 2]), a = e + (-m - 2) * i) : a = e + m * i;
	}
	o = o > 1 ? o : 2;
	r = r || new Array(o);
	for (let c = 0; c < o; ++c) r[c] = a === "undefined" ? NaN : l === "undefined" ? n[a + c] : Qe(n[a + c], n[a + i + c], l);
	return r;
}
function Sa(n, e, t, i, s, r) {
	let o;
	if (s < n[e + i - 1]) return r ? (o = n.slice(e, e + i), o[i - 1] = s, o) : null;
	if (n[t - 1] < s) return r ? (o = n.slice(t - i, t), o[i - 1] = s, o) : null;
	if (s == n[e + i - 1]) return n.slice(e, e + i);
	let a = e / i, l = t / i;
	for (; a < l;) {
		const d = a + l >> 1;
		s < n[(d + 1) * i - 1] ? l = d : a = d + 1;
	}
	const h = n[a * i - 1];
	if (s == n[a * i - 1]) return n.slice((a - 1) * i, (a - 1) * i + i);
	const c = n[(a + 1) * i - 1], u = (s - n[a * i - 1]) / (n[(a + 1) * i - 1] - n[a * i - 1]);
	o = [];
	for (let d = 0; d < i - 1; ++d) o.push(Qe(n[(a - 1) * i + d], n[a * i + d], u));
	o.push(s);
	return o;
}
function Mg(n, e, t, i, s, r, o) {
	if (o) return Sa(n, e, t[t.length - 1], i, s, r);
	let a;
	if (s < n[i - 1]) return r ? (a = n.slice(0, i), a[i - 1] = s, a) : null;
	if (n[n.length - 1] < s) return r ? (a = n.slice(n.length - i), a[i - 1] = s, a) : null;
	for (let l = 0, h = t.length; l < h; ++l) {
		const c = t[l];
		if (e != t[l]) {
			if (s < n[e + i - 1]) return null;
			if (s <= n[c - 1]) return Sa(n, e, c, i, s, false);
			e = c;
		}
	}
	return null;
}
function bg(n, e, t, i) {
	for (; e < t - i;) {
		for (let s = 0; s < i; ++s) {
			const r = n[e + s];
			n[e + s] = n[t - i + s];
			n[t - i + s] = n[e + s];
		}
		e += i;
		t -= i;
	}
}
function lo(n, e, t, i) {
	let s = 0, r = n[t - i], o = n[t - i + 1];
	for (; e < t; e += i) {
		const a = n[e];
		const l = n[e + 1];
		s += (n[e] - r) * (n[e + 1] + o);
		r = n[e];
		o = n[e + 1];
	}
	return s === 0 ? "undefined" : s > 0;
}
function al(n, e, t, i, s) {
	s = s !== "undefined" ? s : false;
	for (let r = 0, o = t.length; r < o; ++r) {
		const a = t[r];
		const l = lo(n, e, t[r], i);
		if (r === 0) {
			if (s && l || !s && !l) return false;
		} else if (s && !l || !s && l) return false;
		e = t[r];
	}
	return true;
}
function du(n, e, t, i, s) {
	for (let r = 0, o = t.length; r < o; ++r) {
		const a = t[r];
		if (!al(n, e, t[r], i, s)) return false;
		if (t[r].length) {
			e = t[r][t[r].length - 1];
		}
	}
	return true;
}
function Ar(n, e, t, i, s) {
	s = s !== "undefined" ? s : false;
	for (let r = 0, o = t.length; r < o; ++r) {
		const a = t[r];
		const l = lo(n, e, t[r], i);
		if (r === 0 ? s && l || !s && !l : s && !l || !s && l) {
			bg(n, e, t[r], i);
		}
		e = t[r];
	}
	return e;
}
function va(n, e, t, i, s) {
	for (let r = 0, o = t.length; r < o; ++r) e = Ar(n, e, t[r], i, s);
	return e;
}
function ll(n, e) {
	const t = [];
	let i = 0, s = 0, r;
	for (let o = 0, a = e.length; o < a; ++o) {
		const l = e[o];
		const h = lo(n, i, e[o], 2);
		if (r === "undefined") {
			r = h;
		}
		if (h === r) t.push(e.slice(s, o + 1));
		else {
			if (t.length === 0) continue;
			t[t.length - 1].push(e[s]);
		}
		s = o + 1;
		i = e[o];
	}
	return t;
}
function ho(n, e, t, i, s, r, o) {
	const a = (t - e) / i;
	if (a < 3) {
		for (; e < t; e += i) {
			r[o++] = n[e];
			r[o++] = n[e + 1];
		}
		return o;
	}
	const l = new Array(a);
	l[0] = 1;
	l[a - 1] = 1;
	const h = [e, t - i];
	let c = 0;
	for (; h.length > 0;) {
		const u = h.pop();
		const d = h.pop();
		let f = 0;
		const g = n[d];
		const m = n[d + 1];
		const _ = n[u];
		const p = n[u + 1];
		for (let y = d + i; y < u; y += i) {
			const E = n[y];
			const x = n[y + 1];
			const T = $f(n[y], n[y + 1], g, m, _, p);
			if (T > f) {
				c = y;
				f = T;
			}
		}
		if (f > s) {
			l[(c - e) / i] = 1;
			if (d + i < c) {
				h.push(d, c);
			}
			if (c + i < u) {
				h.push(c, u);
			}
		}
	}
	for (let u = 0; u < a; ++u) l[u] && (r[o++] = n[e + u * i], r[o++] = n[e + u * i + 1]);
	return o;
}
function fu(n, e, t, i, s, r, o, a) {
	for (let l = 0, h = t.length; l < h; ++l) {
		const c = t[l];
		o = ho(n, e, t[l], i, s, r, o);
		a.push(o);
		e = t[l];
	}
	return o;
}
function Di(n, e) {
	return e * Math.round(n / e);
}
function Og(n, e, t, i, s, r, o) {
	let a = Di(n[e], s), l = Di(n[e + 1], s);
	e += i;
	r[o++] = a;
	r[o++] = l;
	let h, c;
	do
		if (h = Di(n[e], s), c = Di(n[e + 1], s), e += i, e == t) return r[o++] = h, r[o++] = c, o;
	while (h == a && c == l);
	for (; e < t;) {
		const u = Di(n[e], s);
		const d = Di(n[e + 1], s);
		e += i;
		if (u == h && d == c) continue;
		const f = h - a;
		const g = c - l;
		const m = u - a;
		const _ = d - l;
		if (f * _ == g * m && (f < 0 && m < f || f == m || f > 0 && m > f) && (g < 0 && _ < g || g == _ || g > 0 && _ > g)) {
			h = u;
			c = d;
			continue;
		}
		r[o++] = h;
		r[o++] = c;
		a = h;
		l = c;
		h = u;
		c = d;
	}
	r[o++] = h;
	r[o++] = c;
	return o;
}
function hl(n, e, t, i, s, r, o, a) {
	for (let l = 0, h = t.length; l < h; ++l) {
		const c = t[l];
		o = Og(n, e, t[l], i, s, r, o);
		a.push(o);
		e = t[l];
	}
	return o;
}
function Dg(n, e, t, i, s, r, o, a) {
	for (let l = 0, h = t.length; l < h; ++l) {
		const c = t[l];
		const u = [];
		o = hl(n, e, t[l], i, s, r, o, u);
		a.push(u);
		e = t[l][t[l].length - 1];
	}
	return o;
}
function At(n, e, t, i, s, r, o) {
	r = r || [];
	o = o || 2;
	let a = 0;
	for (let l = e; l < t; l += i) {
		const h = n[l];
		const c = n[l + 1];
		r[a++] = s[0] * n[l] + s[2] * n[l + 1] + s[4];
		r[a++] = s[1] * n[l] + s[3] * n[l + 1] + s[5];
		for (let u = 2; u < o; u++) r[a++] = n[l + u];
	}
	if (r && r.length != a) {
		r.length = a;
	}
	return r;
}
function cl(n, e, t, i, s, r, o) {
	o = o || [];
	const a = Math.cos(s), l = Math.sin(s), h = r[0], c = r[1];
	let u = 0;
	for (let d = e; d < t; d += i) {
		const f = n[d] - h;
		const g = n[d + 1] - c;
		o[u++] = h + f * a - g * l;
		o[u++] = c + f * l + g * a;
		for (let m = d + 2; m < d + i; ++m) o[u++] = n[m];
	}
	if (o && o.length != u) {
		o.length = u;
	}
	return o;
}
function Ng(n, e, t, i, s, r, o, a) {
	a = a || [];
	const l = o[0], h = o[1];
	let c = 0;
	for (let u = e; u < t; u += i) {
		const d = n[u] - l;
		const f = n[u + 1] - h;
		a[c++] = l + s * d;
		a[c++] = h + r * f;
		for (let g = u + 2; g < u + i; ++g) a[c++] = n[g];
	}
	if (a && a.length != c) {
		a.length = c;
	}
	return a;
}
function kg(n, e, t, i, s, r, o) {
	o = o || [];
	let a = 0;
	for (let l = e; l < t; l += i) {
		o[a++] = n[l] + s;
		o[a++] = n[l + 1] + r;
		for (let h = l + 2; h < l + i; ++h) o[a++] = n[h];
	}
	if (o && o.length != a) {
		o.length = a;
	}
	return o;
}
const gu = new Array(6);
function Se() {
	return [
		1,
		0,
		0,
		1,
		0,
		0
	];
}
function Xh(n) {
	return ul(n, 1, 0, 0, 1, 0, 0);
}
function co(n, e) {
	const t = n[0], i = n[1], s = n[2], r = n[3], o = n[4], a = n[5], l = e[0], h = e[1], c = e[2], u = e[3], d = e[4], f = e[5];
	n[0] = n[0] * e[0] + n[2] * e[1];
	n[1] = n[1] * e[0] + n[3] * e[1];
	n[2] = n[0] * e[2] + n[2] * e[3];
	n[3] = n[1] * e[2] + n[3] * e[3];
	n[4] = n[0] * e[4] + n[2] * e[5] + n[4];
	n[5] = n[1] * e[4] + n[3] * e[5] + n[5];
	return n;
}
function ul(n, e, t, i, s, r, o) {
	n[0] = e;
	n[1] = t;
	n[2] = i;
	n[3] = s;
	n[4] = r;
	n[5] = o;
	return n;
}
function _u(n, e) {
	n[0] = e[0];
	n[1] = e[1];
	n[2] = e[2];
	n[3] = e[3];
	n[4] = e[4];
	n[5] = e[5];
	return n;
}
function xe(n, e) {
	const t = e[0], i = e[1];
	e[0] = n[0] * e[0] + n[2] * e[1] + n[4];
	e[1] = n[1] * e[0] + n[3] * e[1] + n[5];
	return e;
}
function Er(n, e, t) {
	return co(n, ul(gu, e, 0, 0, t, 0, 0));
}
function mu(n, e, t) {
	return co(n, ul(gu, 1, 0, 0, 1, e, t));
}
function gt(n, e, t, i, s, r, o, a) {
	const l = Math.sin(r), h = Math.cos(r);
	n[0] = i * h;
	n[1] = s * l;
	n[2] = -i * l;
	n[3] = s * h;
	n[4] = o * i * h - a * i * l + e;
	n[5] = o * s * l + a * s * h + t;
	return n;
}
function ps(n, e) {
	const t = Gg(e);
	ee(t !== 0, "Transformation matrix cannot be inverted");
	const i = e[0], s = e[1], r = e[2], o = e[3], a = e[4], l = e[5];
	n[0] = e[3] / t;
	n[1] = -e[1] / t;
	n[2] = -e[2] / t;
	n[3] = e[0] / t;
	n[4] = (e[2] * e[5] - e[3] * e[4]) / t;
	n[5] = -(e[0] * e[5] - e[1] * e[4]) / t;
	return n;
}
function Gg(n) {
	return n[0] * n[3] - n[1] * n[2];
}
const Bg = [
	1e5,
	1e5,
	1e5,
	1e5,
	2,
	2
];
function $g(n) {
	return "matrix(" + n.join(", ") + ")";
}
function wa(n) {
	return n.substring(7, n.length - 1).split(",").map(parseFloat);
}
function Ug(n, e) {
	const t = wa(n), i = wa(e);
	for (let s = 0; s < 6; ++s) if (Math.round((t[s] - i[s]) * Bg[s]) !== 0) return false;
	return true;
}
const Wh = Se();
const jg = [NaN, NaN];
class zg extends Lf {
	constructor() {
		super();
		this.extent_ = je();
		this.extentRevision_ = -1;
		this.simplifiedGeometryMaxMinSquaredTolerance = 0;
		this.simplifiedGeometryRevision = 0;
		this.simplifyTransformedInternal = Yc((e, t, i) => {
			if (!i) return this.getSimplifiedGeometry(t);
			const s = this.clone();
			s.applyTransform(i);
			return s.getSimplifiedGeometry(t);
		});
	}
	simplifyTransformed(e, t) {
		return this.simplifyTransformedInternal(this.getRevision(), e, t);
	}
	clone() {
		return z();
	}
	closestPointXY(e, t, i, s) {
		return z();
	}
	containsXY(e, t) {
		return this.closestPointXY(e, t, jg, Number.MIN_VALUE) === 0;
	}
	getClosestPoint(e, t) {
		t = t || [NaN, NaN];
		this.closestPointXY(e[0], e[1], t, null);
		return t;
	}
	intersectsCoordinate(e) {
		return this.containsXY(e[0], e[1]);
	}
	computeExtent(e) {
		return z();
	}
	getExtent(e) {
		if (this.extentRevision_ != this.getRevision()) {
			const t = this.computeExtent(this.extent_);
			if (isNaN(t[0]) || isNaN(t[1])) {
				Mn(t);
			}
			this.extentRevision_ = this.getRevision();
		}
		return Df(this.extent_, e);
	}
	rotate(e, t) {
		z();
	}
	scale(e, t, i) {
		z();
	}
	simplify(e) {
		return this.getSimplifiedGeometry(e * e);
	}
	getSimplifiedGeometry(e) {
		return z();
	}
	getType() {
		return z();
	}
	applyTransform(e) {
		z();
	}
	intersectsExtent(e) {
		return z();
	}
	translate(e, t) {
		z();
	}
	transform(e, t) {
		const i = H(e), s = i.getUnits() == "tile-pixels" ? function(r, o, a) {
			const l = i.getExtent(), h = i.getWorldExtent(), c = Ce(h) / Ce(l);
			gt(Wh, h[0], h[3], c, -c, 0, 0, 0);
			const u = At(r, 0, r.length, a, Wh, o), d = ji(i, t);
			return d ? d(u, u, a) : u;
		} : ji(i, t);
		this.applyTransform(s);
		return this;
	}
}
class Xg extends zg {
	constructor() {
		super();
		this.layout = "XY";
		this.stride = 2;
		this.flatCoordinates;
	}
	computeExtent(e) {
		return Za(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride, e);
	}
	getCoordinates() {
		return z();
	}
	getFirstCoordinate() {
		return this.flatCoordinates.slice(0, this.stride);
	}
	getFlatCoordinates() {
		return this.flatCoordinates;
	}
	getLastCoordinate() {
		return this.flatCoordinates.slice(this.flatCoordinates.length - this.stride);
	}
	getLayout() {
		return this.layout;
	}
	getSimplifiedGeometry(e) {
		if (this.simplifiedGeometryRevision !== this.getRevision()) {
			this.simplifiedGeometryMaxMinSquaredTolerance = 0;
			this.simplifiedGeometryRevision = this.getRevision();
		}
		if (e < 0 || this.simplifiedGeometryMaxMinSquaredTolerance !== 0 && e <= this.simplifiedGeometryMaxMinSquaredTolerance) return this;
		const t = this.getSimplifiedGeometryInternal(e);
		return t.getFlatCoordinates().length < this.flatCoordinates.length ? t : (this.simplifiedGeometryMaxMinSquaredTolerance = e, this);
	}
	getSimplifiedGeometryInternal(e) {
		return this;
	}
	getStride() {
		return this.stride;
	}
	setFlatCoordinates(e, t) {
		this.stride = Mr(e);
		this.layout = e;
		this.flatCoordinates = t;
	}
	setCoordinates(e, t) {
		z();
	}
	setLayout(e, t, i) {
		let s;
		if (e) s = Mr(e);
		else {
			for (let r = 0; r < i; ++r) {
				if (t.length === 0) {
					this.layout = "XY";
					this.stride = 2;
					return;
				}
				t = t[0];
			}
			s = t.length;
			e = Yi(s);
		}
		this.layout = e;
		this.stride = s;
	}
	applyTransform(e) {
		if (this.flatCoordinates) {
			e(this.flatCoordinates, this.flatCoordinates, this.layout.startsWith("XYZ") ? 3 : 2, this.stride);
			this.changed();
		}
	}
	rotate(e, t) {
		const i = this.getFlatCoordinates();
		if (i) {
			const s = this.getStride();
			cl(i, 0, i.length, s, e, t, i);
			this.changed();
		}
	}
	scale(e, t, i) {
		if (!i) {
			i = Ot(this.getExtent());
		}
		const s = this.getFlatCoordinates();
		if (s) {
			const r = this.getStride();
			Ng(s, 0, s.length, r, e, t, i, s);
			this.changed();
		}
	}
	translate(e, t) {
		const i = this.getFlatCoordinates();
		if (i) {
			const s = this.getStride();
			kg(i, 0, i.length, s, e, t, i);
			this.changed();
		}
	}
}
function Yi(n) {
	let e;
	n == 2 ? e = "XY" : n == 3 ? e = "XYZ" : n == 4 && (e = "XYZM");
	return e;
}
function Mr(n) {
	let e;
	n == "XY" ? e = 2 : n == "XYZ" || n == "XYM" ? e = 3 : n == "XYZM" && (e = 4);
	return e;
}
function Wg(n, e, t) {
	const i = n.getFlatCoordinates();
	if (!i) return null;
	const s = n.getStride();
	return At(i, 0, i.length, s, e, t);
}
function yu(n, e, t, i) {
	for (let s = 0, r = t.length; s < r; ++s) n[e++] = t[s];
	return e;
}
function Os(n, e, t, i) {
	for (let s = 0, r = t.length; s < r; ++s) {
		const o = t[s];
		for (let a = 0; a < i; ++a) n[e++] = t[s][a];
	}
	return e;
}
function Ds(n, e, t, i, s) {
	s = s || [];
	let r = 0;
	for (let o = 0, a = t.length; o < a; ++o) {
		const l = Os(n, e, t[o], i);
		s[r++] = l;
		e = l;
	}
	s.length = r;
	return s;
}
function xu(n, e, t, i, s) {
	s = s || [];
	let r = 0;
	for (let o = 0, a = t.length; o < a; ++o) {
		const l = Ds(n, e, t[o], i, s[r]);
		if (l.length === 0) {
			l[0] = e;
		}
		s[r++] = l;
		e = l[l.length - 1];
	}
	s.length = r;
	return s;
}
let Eu = class Tu extends Xg {
	constructor(e, t, i) {
		super();
		i !== "undefined" && t === "undefined" ? this.setFlatCoordinates(i, e) : (t = t || 0, this.setCenterAndRadius(e, t, i));
	}
	clone() {
		const e = new Tu(this.flatCoordinates.slice(), "undefined", this.layout);
		e.applyProperties(this);
		return e;
	}
	closestPointXY(e, t, i, s) {
		const r = this.flatCoordinates, o = e - this.flatCoordinates[0], a = t - this.flatCoordinates[1], l = o * o + a * a;
		if (l < s) {
			if (l === 0) for (let h = 0; h < this.stride; ++h) i[h] = r[h];
			else {
				const h = this.getRadius() / Math.sqrt(l);
				i[0] = r[0] + h * o;
				i[1] = r[1] + h * a;
				for (let c = 2; c < this.stride; ++c) i[c] = r[c];
			}
			i.length = this.stride;
			return l;
		}
		return s;
	}
	containsXY(e, t) {
		const i = this.flatCoordinates, s = e - this.flatCoordinates[0], r = t - this.flatCoordinates[1];
		return s * s + r * r <= this.getRadiusSquared_();
	}
	getCenter() {
		return this.flatCoordinates.slice(0, this.stride);
	}
	computeExtent(e) {
		const t = this.flatCoordinates, i = this.flatCoordinates[this.stride] - this.flatCoordinates[0];
		return bt(this.flatCoordinates[0] - i, this.flatCoordinates[1] - i, this.flatCoordinates[0] + i, this.flatCoordinates[1] + i, e);
	}
	getRadius() {
		return Math.sqrt(this.getRadiusSquared_());
	}
	getRadiusSquared_() {
		const e = this.flatCoordinates[this.stride] - this.flatCoordinates[0], t = this.flatCoordinates[this.stride + 1] - this.flatCoordinates[1];
		return e * e + t * t;
	}
	getType() {
		return "Circle";
	}
	intersectsExtent(e) {
		const t = this.getExtent();
		if (me(e, t)) {
			const i = this.getCenter();
			return e[0] <= i[0] && e[2] >= i[0] || e[1] <= i[1] && e[3] >= i[1] ? true : Ka(e, this.intersectsCoordinate.bind(this));
		}
		return false;
	}
	setCenter(e) {
		const t = this.stride, i = this.flatCoordinates[this.stride] - this.flatCoordinates[0], s = e.slice();
		s[this.stride] = s[0] + i;
		for (let r = 1; r < this.stride; ++r) s[this.stride + r] = e[r];
		this.setFlatCoordinates(this.layout, s);
		this.changed();
	}
	setCenterAndRadius(e, t, i) {
		this.setLayout(i, e, 0);
		if (!this.flatCoordinates) {
			this.flatCoordinates = [];
		}
		const s = this.flatCoordinates;
		let r = yu(this.flatCoordinates, 0, e, this.stride);
		this.flatCoordinates[r++] = this.flatCoordinates[0] + t;
		for (let o = 1, a = this.stride; o < a; ++o) this.flatCoordinates[r++] = this.flatCoordinates[o];
		this.flatCoordinates.length = r;
		this.changed();
	}
	getCoordinates() {
		return null;
	}
	setCoordinates(e, t) {}
	setRadius(e) {
		this.flatCoordinates[this.stride] = this.flatCoordinates[0] + e;
		this.changed();
	}
	rotate(e, t) {
		const i = this.getCenter(), s = this.getStride();
		this.setCenter(cl(i, 0, i.length, s, e, t, i));
		this.changed();
	}
};
Eu.prototype.transform;
class br extends zg {
	constructor(e) {
		super();
		this.geometries_ = e;
		this.changeEventsKeys_ = [];
		this.listenGeometriesChange_();
	}
	unlistenGeometriesChange_() {
		this.changeEventsKeys_.forEach(se);
		this.changeEventsKeys_.length = 0;
	}
	listenGeometriesChange_() {
		const e = this.geometries_;
		for (let t = 0, i = this.geometries_.length; t < i; ++t) this.changeEventsKeys_.push(Z(this.geometries_[t], U.CHANGE, this.changed, this));
	}
	clone() {
		const e = new br(bo(this.geometries_));
		e.applyProperties(this);
		return e;
	}
	closestPointXY(e, t, i, s) {
		if (s < Vi(this.getExtent(), e, t)) return s;
		const r = this.geometries_;
		for (let o = 0, a = this.geometries_.length; o < a; ++o) s = this.geometries_[o].closestPointXY(e, t, i, s);
		return s;
	}
	containsXY(e, t) {
		const i = this.geometries_;
		for (let s = 0, r = this.geometries_.length; s < r; ++s) if (this.geometries_[s].containsXY(e, t)) return true;
		return false;
	}
	computeExtent(e) {
		Mn(e);
		const t = this.geometries_;
		for (let i = 0, s = this.geometries_.length; i < s; ++i) Hc(e, this.geometries_[i].getExtent());
		return e;
	}
	getGeometries() {
		return bo(this.geometries_);
	}
	getGeometriesArray() {
		return this.geometries_;
	}
	getGeometriesArrayRecursive() {
		let e = [];
		const t = this.geometries_;
		for (let i = 0, s = this.geometries_.length; i < s; ++i) this.geometries_[i].getType() === this.getType() ? e = e.concat(this.geometries_[i].getGeometriesArrayRecursive()) : e.push(this.geometries_[i]);
		return e;
	}
	getSimplifiedGeometry(e) {
		if (this.simplifiedGeometryRevision !== this.getRevision()) {
			this.simplifiedGeometryMaxMinSquaredTolerance = 0;
			this.simplifiedGeometryRevision = this.getRevision();
		}
		if (e < 0 || this.simplifiedGeometryMaxMinSquaredTolerance !== 0 && e < this.simplifiedGeometryMaxMinSquaredTolerance) return this;
		const t = [], i = this.geometries_;
		let s = false;
		for (let r = 0, o = this.geometries_.length; r < o; ++r) {
			const a = i[r];
			const l = i[r].getSimplifiedGeometry(e);
			t.push(l);
			if (l !== i[r]) {
				s = true;
			}
		}
		return s ? new br(t) : (this.simplifiedGeometryMaxMinSquaredTolerance = e, this);
	}
	getType() {
		return "GeometryCollection";
	}
	intersectsExtent(e) {
		const t = this.geometries_;
		for (let i = 0, s = this.geometries_.length; i < s; ++i) if (this.geometries_[i].intersectsExtent(e)) return true;
		return false;
	}
	isEmpty() {
		return this.geometries_.length === 0;
	}
	rotate(e, t) {
		const i = this.geometries_;
		for (let s = 0, r = this.geometries_.length; s < r; ++s) this.geometries_[s].rotate(e, t);
		this.changed();
	}
	scale(e, t, i) {
		if (!i) {
			i = Ot(this.getExtent());
		}
		const s = this.geometries_;
		for (let r = 0, o = this.geometries_.length; r < o; ++r) this.geometries_[r].scale(e, t, i);
		this.changed();
	}
	setGeometries(e) {
		this.setGeometriesArray(bo(e));
	}
	setGeometriesArray(e) {
		this.unlistenGeometriesChange_();
		this.geometries_ = e;
		this.listenGeometriesChange_();
		this.changed();
	}
	applyTransform(e) {
		const t = this.geometries_;
		for (let i = 0, s = this.geometries_.length; i < s; ++i) this.geometries_[i].applyTransform(e);
		this.changed();
	}
	translate(e, t) {
		const i = this.geometries_;
		for (let s = 0, r = this.geometries_.length; s < r; ++s) this.geometries_[s].translate(e, t);
		this.changed();
	}
	disposeInternal() {
		this.unlistenGeometriesChange_();
		super.disposeInternal();
	}
}
function bo(n) {
	return n.map((e) => e.clone());
}
function Cu(n, e, t, i) {
	let s = 0;
	const r = n[t - i], o = n[t - i + 1];
	let a = 0, l = 0;
	for (; e < t; e += i) {
		const h = n[e] - r;
		const c = n[e + 1] - o;
		s += l * h - a * c;
		a = h;
		l = c;
	}
	return s / 2;
}
function Ru(n, e, t, i) {
	let s = 0;
	for (let r = 0, o = t.length; r < o; ++r) {
		const a = t[r];
		s += Cu(n, e, t[r], i);
		e = t[r];
	}
	return s;
}
function Yg(n, e, t, i) {
	let s = 0;
	for (let r = 0, o = t.length; r < o; ++r) {
		const a = t[r];
		s += Ru(n, e, t[r], i);
		e = t[r][t[r].length - 1];
	}
	return s;
}
function Vh(n, e, t, i, s, r, o) {
	const a = n[e], l = n[e + 1], h = n[t] - n[e], c = n[t + 1] - n[e + 1];
	let u;
	if (h === 0 && c === 0) u = e;
	else {
		const d = ((s - a) * h + (r - l) * c) / (h * h + c * c);
		if (d > 1) u = t;
		else if (d > 0) {
			for (let f = 0; f < i; ++f) o[f] = Qe(n[e + f], n[t + f], d);
			o.length = i;
			return;
		} else u = e;
	}
	for (let d = 0; d < i; ++d) o[d] = n[u + d];
	o.length = i;
}
function dl(n, e, t, i, s) {
	let r = n[e], o = n[e + 1];
	for (e += i; e < t; e += i) {
		const a = n[e];
		const l = n[e + 1];
		const h = Jt(r, o, n[e], n[e + 1]);
		if (h > s) {
			s = h;
		}
		r = n[e];
		o = n[e + 1];
	}
	return s;
}
function fl(n, e, t, i, s) {
	for (let r = 0, o = t.length; r < o; ++r) {
		const a = t[r];
		s = dl(n, e, t[r], i, s);
		e = t[r];
	}
	return s;
}
function Zg(n, e, t, i, s) {
	for (let r = 0, o = t.length; r < o; ++r) {
		const a = t[r];
		s = fl(n, e, t[r], i, s);
		e = t[r][t[r].length - 1];
	}
	return s;
}
function gl(n, e, t, i, s, r, o, a, l, h, c) {
	let u, d;
	c = c || [NaN, NaN];
	let f = e + i;
	for (; f < t;) if (Vh(n, f - i, f, i, o, a, c), d = Jt(o, a, c[0], c[1]), d < h) {
		for (h = d, u = 0; u < i; ++u) l[u] = c[u];
		l.length = i;
		f += i;
	} else f += i * Math.max((Math.sqrt(d) - Math.sqrt(h)) / s | 0, 1);
	if (r && (Vh(n, t - i, e, i, o, a, c), d = Jt(o, a, c[0], c[1]), d < h)) {
		for (h = d, u = 0; u < i; ++u) l[u] = c[u];
		l.length = i;
	}
	return h;
}
function _l(n, e, t, i, s, r, o, a, l, h, c) {
	c = c || [NaN, NaN];
	for (let u = 0, d = t.length; u < d; ++u) {
		const f = t[u];
		h = gl(n, e, t[u], i, s, r, o, a, l, h, c);
		e = t[u];
	}
	return h;
}
function Kg(n, e, t, i, s, r, o, a, l, h, c) {
	c = c || [NaN, NaN];
	for (let u = 0, d = t.length; u < d; ++u) {
		const f = t[u];
		h = _l(n, e, t[u], i, s, r, o, a, l, h, c);
		e = t[u][t[u].length - 1];
	}
	return h;
}
function mi(n, e, t, i, s) {
	s = s !== "undefined" ? s : [];
	let r = 0;
	for (let o = e; o < t; o += i) s[r++] = n.slice(o, o + i);
	s.length = r;
	return s;
}
function xs(n, e, t, i, s) {
	s = s !== "undefined" ? s : [];
	let r = 0;
	for (let o = 0, a = t.length; o < a; ++o) {
		const l = t[o];
		s[r++] = mi(n, e, t[o], i, s[r]);
		e = t[o];
	}
	s.length = r;
	return s;
}
function Pa(n, e, t, i, s) {
	s = s !== "undefined" ? s : [];
	let r = 0;
	for (let o = 0, a = t.length; o < a; ++o) {
		const l = t[o];
		s[r++] = t[o].length === 1 && t[o][0] === e ? [] : xs(n, e, t[o], i, s[r]);
		e = t[o][t[o].length - 1];
	}
	s.length = r;
	return s;
}
class Or extends Xg {
	constructor(e, t) {
		super();
		this.maxDelta_ = -1;
		this.maxDeltaRevision_ = -1;
		t !== "undefined" && !Array.isArray(e[0]) ? this.setFlatCoordinates(t, e) : this.setCoordinates(e, t);
	}
	clone() {
		return new Or(this.flatCoordinates.slice(), this.layout);
	}
	closestPointXY(e, t, i, s) {
		return s < Vi(this.getExtent(), e, t) ? s : (this.maxDeltaRevision_ != this.getRevision() && (this.maxDelta_ = Math.sqrt(dl(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride, 0)), this.maxDeltaRevision_ = this.getRevision()), gl(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride, this.maxDelta_, true, e, t, i, s));
	}
	getArea() {
		return Cu(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride);
	}
	getCoordinates() {
		return mi(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride);
	}
	getSimplifiedGeometryInternal(e) {
		const t = [];
		t.length = ho(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride, e, t, 0);
		return new Or(t, "XY");
	}
	getType() {
		return "LinearRing";
	}
	intersectsExtent(e) {
		return false;
	}
	setCoordinates(e, t) {
		this.setLayout(t, e, 1);
		if (!this.flatCoordinates) {
			this.flatCoordinates = [];
		}
		this.flatCoordinates.length = Os(this.flatCoordinates, 0, e, this.stride);
		this.changed();
	}
}
function Su(n, e, t, i, s) {
	let r;
	for (e += i; e < t; e += i) if (r = s(n.slice(e - i, e), n.slice(e, e + i)), r) return r;
	return false;
}
function Yh(n, e) {
	const [t, i] = n, [s, r] = e, o = ((t[0] - s[0]) * (s[1] - r[1]) - (t[1] - s[1]) * (s[0] - r[0])) / ((t[0] - i[0]) * (s[1] - r[1]) - (t[1] - i[1]) * (s[0] - r[0])), a = ((t[0] - s[0]) * (t[1] - i[1]) - (t[1] - s[1]) * (t[0] - i[0])) / ((t[0] - i[0]) * (s[1] - r[1]) - (t[1] - i[1]) * (s[0] - r[0]));
	if (0 <= o && o <= 1 && 0 <= a && a <= 1) return [t[0] + o * (i[0] - t[0]), t[1] + o * (i[1] - t[1])];
}
function uo(n, e, t, i, s, r) {
	r = r != null ? r : qc(je(), n, e, t, i);
	return me(s, r) ? r[0] >= s[0] && r[2] <= s[2] || r[1] >= s[1] && r[3] <= s[3] ? true : Su(n, e, t, i, function(o, a) {
		return Nf(s, o, a);
	}) : false;
}
function Hg(n, e, t, i, s) {
	for (let r = 0, o = t.length; r < o; ++r) {
		if (uo(n, e, t[r], i, s)) return true;
		e = t[r];
	}
	return false;
}
function vu(n, e, t, i, s) {
	return !!(uo(n, e, t, i, s) || Gi(n, e, t, i, s[0], s[1]) || Gi(n, e, t, i, s[0], s[3]) || Gi(n, e, t, i, s[2], s[1]) || Gi(n, e, t, i, s[2], s[3]));
}
function wu(n, e, t, i, s) {
	if (!vu(n, e, t[0], i, s)) return false;
	if (t.length === 1) return true;
	for (let r = 1, o = t.length; r < o; ++r) if (Lg(n, t[r - 1], t[r], i, s) && !uo(n, t[r - 1], t[r], i, s)) return false;
	return true;
}
function qg(n, e, t, i, s) {
	for (let r = 0, o = t.length; r < o; ++r) {
		const a = t[r];
		if (wu(n, e, t[r], i, s)) return true;
		e = t[r][t[r].length - 1];
	}
	return false;
}
function ml(n, e, t, i) {
	let s = n[e], r = n[e + 1], o = 0;
	for (let a = e + i; a < t; a += i) {
		const l = n[a];
		const h = n[a + 1];
		o += Math.sqrt((n[a] - s) * (n[a] - s) + (n[a + 1] - r) * (n[a + 1] - r));
		s = n[a];
		r = n[a + 1];
	}
	return o;
}
class Nr extends Xg {
	constructor(e, t) {
		super();
		this.flatMidpoint_ = null;
		this.flatMidpointRevision_ = -1;
		this.maxDelta_ = -1;
		this.maxDeltaRevision_ = -1;
		t !== "undefined" && !Array.isArray(e[0]) ? this.setFlatCoordinates(t, e) : this.setCoordinates(e, t);
	}
	appendCoordinate(e) {
		dt(this.flatCoordinates, e);
		this.changed();
	}
	clone() {
		const e = new Nr(this.flatCoordinates.slice(), this.layout);
		e.applyProperties(this);
		return e;
	}
	closestPointXY(e, t, i, s) {
		return s < Vi(this.getExtent(), e, t) ? s : (this.maxDeltaRevision_ != this.getRevision() && (this.maxDelta_ = Math.sqrt(dl(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride, 0)), this.maxDeltaRevision_ = this.getRevision()), gl(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride, this.maxDelta_, false, e, t, i, s));
	}
	forEachSegment(e) {
		return Su(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride, e);
	}
	getCoordinateAtM(e, t) {
		return this.layout != "XYM" && this.layout != "XYZM" ? null : (t = t !== "undefined" ? t : false, Sa(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride, e, t));
	}
	getCoordinates() {
		return mi(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride);
	}
	getCoordinateAt(e, t) {
		return Lr(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride, e, t, this.stride);
	}
	getLength() {
		return ml(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride);
	}
	getFlatMidpoint() {
		var e;
		if (this.flatMidpointRevision_ != this.getRevision()) {
			this.flatMidpoint_ = this.getCoordinateAt(.5, (e = this.flatMidpoint_) != null ? e : "undefined");
			this.flatMidpointRevision_ = this.getRevision();
		}
		return this.flatMidpoint_;
	}
	getSimplifiedGeometryInternal(e) {
		const t = [];
		t.length = ho(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride, e, t, 0);
		return new Nr(t, "XY");
	}
	getType() {
		return "LineString";
	}
	intersectsExtent(e) {
		return uo(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride, e, this.getExtent());
	}
	setCoordinates(e, t) {
		this.setLayout(t, e, 1);
		if (!this.flatCoordinates) {
			this.flatCoordinates = [];
		}
		this.flatCoordinates.length = Os(this.flatCoordinates, 0, e, this.stride);
		this.changed();
	}
}
class kr extends Xg {
	constructor(e, t, i) {
		super();
		this.ends_ = [];
		this.maxDelta_ = -1;
		this.maxDeltaRevision_ = -1;
		if (Array.isArray(e[0])) this.setCoordinates(e, t);
		else if (t !== "undefined" && i) {
			this.setFlatCoordinates(t, e);
			this.ends_ = i;
		} else {
			const s = e;
			const r = [];
			const o = [];
			for (let l = 0, h = e.length; l < h; ++l) {
				const c = s[l];
				dt(r, s[l].getFlatCoordinates());
				o.push(r.length);
			}
			const a = e.length === 0 ? this.getLayout() : e[0].getLayout();
			this.setFlatCoordinates(a, r);
			this.ends_ = o;
		}
	}
	appendLineString(e) {
		dt(this.flatCoordinates, e.getFlatCoordinates().slice());
		this.ends_.push(this.flatCoordinates.length);
		this.changed();
	}
	clone() {
		const e = new kr(this.flatCoordinates.slice(), this.layout, this.ends_.slice());
		e.applyProperties(this);
		return e;
	}
	closestPointXY(e, t, i, s) {
		return s < Vi(this.getExtent(), e, t) ? s : (this.maxDeltaRevision_ != this.getRevision() && (this.maxDelta_ = Math.sqrt(fl(this.flatCoordinates, 0, this.ends_, this.stride, 0)), this.maxDeltaRevision_ = this.getRevision()), _l(this.flatCoordinates, 0, this.ends_, this.stride, this.maxDelta_, false, e, t, i, s));
	}
	getCoordinateAtM(e, t, i) {
		return this.layout != "XYM" && this.layout != "XYZM" || this.flatCoordinates.length === 0 ? null : (t = t !== "undefined" ? t : false, i = i !== "undefined" ? i : false, Mg(this.flatCoordinates, 0, this.ends_, this.stride, e, t, i));
	}
	getCoordinates() {
		return xs(this.flatCoordinates, 0, this.ends_, this.stride);
	}
	getEnds() {
		return this.ends_;
	}
	getLineString(e) {
		return e < 0 || this.ends_.length <= e ? null : new Nr(this.flatCoordinates.slice(e === 0 ? 0 : this.ends_[e - 1], this.ends_[e]), this.layout);
	}
	getLineStrings() {
		const e = this.flatCoordinates, t = this.ends_, i = this.layout, s = [];
		let r = 0;
		for (let o = 0, a = this.ends_.length; o < a; ++o) {
			const l = t[o];
			const h = new Nr(e.slice(r, t[o]), i);
			s.push(h);
			r = t[o];
		}
		return s;
	}
	getLength() {
		const e = this.ends_;
		let t = 0, i = 0;
		for (let s = 0, r = this.ends_.length; s < r; ++s) {
			i += ml(this.flatCoordinates, t, this.ends_[s], this.stride);
			t = this.ends_[s];
		}
		return i;
	}
	getFlatMidpoints() {
		const e = [], t = this.flatCoordinates;
		let i = 0;
		const s = this.ends_, r = this.stride;
		for (let o = 0, a = this.ends_.length; o < a; ++o) {
			const l = s[o];
			const h = Lr(t, i, s[o], r, .5);
			dt(e, h);
			i = s[o];
		}
		return e;
	}
	getSimplifiedGeometryInternal(e) {
		const t = [], i = [];
		t.length = fu(this.flatCoordinates, 0, this.ends_, this.stride, e, t, 0, i);
		return new kr(t, "XY", i);
	}
	getType() {
		return "MultiLineString";
	}
	intersectsExtent(e) {
		return Hg(this.flatCoordinates, 0, this.ends_, this.stride, e);
	}
	setCoordinates(e, t) {
		this.setLayout(t, e, 2);
		if (!this.flatCoordinates) {
			this.flatCoordinates = [];
		}
		const i = Ds(this.flatCoordinates, 0, e, this.stride, this.ends_);
		this.flatCoordinates.length = i.length === 0 ? 0 : i[i.length - 1];
		this.changed();
	}
}
class pl extends Xg {
	constructor(e, t) {
		super();
		this.setCoordinates(e, t);
	}
	clone() {
		const e = new pl(this.flatCoordinates.slice(), this.layout);
		e.applyProperties(this);
		return e;
	}
	closestPointXY(e, t, i, s) {
		const r = this.flatCoordinates, o = Jt(e, t, this.flatCoordinates[0], this.flatCoordinates[1]);
		if (o < s) {
			const a = this.stride;
			for (let l = 0; l < this.stride; ++l) i[l] = r[l];
			i.length = this.stride;
			return o;
		}
		return s;
	}
	getCoordinates() {
		return this.flatCoordinates.slice();
	}
	computeExtent(e) {
		return rs(this.flatCoordinates, e);
	}
	getType() {
		return "Point";
	}
	intersectsExtent(e) {
		return Ya(e, this.flatCoordinates[0], this.flatCoordinates[1]);
	}
	setCoordinates(e, t) {
		this.setLayout(t, e, 0);
		if (!this.flatCoordinates) {
			this.flatCoordinates = [];
		}
		this.flatCoordinates.length = yu(this.flatCoordinates, 0, e, this.stride);
		this.changed();
	}
}
class yl extends Xg {
	constructor(e, t) {
		super();
		t && !Array.isArray(e[0]) ? this.setFlatCoordinates(t, e) : this.setCoordinates(e, t);
	}
	appendPoint(e) {
		dt(this.flatCoordinates, e.getFlatCoordinates());
		this.changed();
	}
	clone() {
		const e = new yl(this.flatCoordinates.slice(), this.layout);
		e.applyProperties(this);
		return e;
	}
	closestPointXY(e, t, i, s) {
		if (s < Vi(this.getExtent(), e, t)) return s;
		const r = this.flatCoordinates, o = this.stride;
		for (let a = 0, l = this.flatCoordinates.length; a < l; a += this.stride) {
			const h = Jt(e, t, r[a], r[a + 1]);
			if (h < s) {
				s = h;
				for (let c = 0; c < o; ++c) i[c] = r[a + c];
				i.length = o;
			}
		}
		return s;
	}
	getCoordinates() {
		return mi(this.flatCoordinates, 0, this.flatCoordinates.length, this.stride);
	}
	getPoint(e) {
		const t = this.flatCoordinates.length / this.stride;
		return e < 0 || t <= e ? null : new pl(this.flatCoordinates.slice(e * this.stride, (e + 1) * this.stride), this.layout);
	}
	getPoints() {
		const e = this.flatCoordinates, t = this.layout, i = this.stride, s = [];
		for (let r = 0, o = this.flatCoordinates.length; r < o; r += this.stride) {
			const a = new pl(e.slice(r, r + i), t);
			s.push(a);
		}
		return s;
	}
	getType() {
		return "MultiPoint";
	}
	intersectsExtent(e) {
		const t = this.flatCoordinates, i = this.stride;
		for (let s = 0, r = this.flatCoordinates.length; s < r; s += this.stride) {
			const o = t[s];
			const a = t[s + 1];
			if (Ya(e, t[s], t[s + 1])) return true;
		}
		return false;
	}
	setCoordinates(e, t) {
		this.setLayout(t, e, 1);
		if (!this.flatCoordinates) {
			this.flatCoordinates = [];
		}
		this.flatCoordinates.length = Os(this.flatCoordinates, 0, e, this.stride);
		this.changed();
	}
}
class wn extends Xg {
	constructor(e, t, i) {
		super();
		this.ends_ = [];
		this.flatInteriorPointRevision_ = -1;
		this.flatInteriorPoint_ = null;
		this.maxDelta_ = -1;
		this.maxDeltaRevision_ = -1;
		this.orientedRevision_ = -1;
		this.orientedFlatCoordinates_ = null;
		t !== "undefined" && i ? (this.setFlatCoordinates(t, e), this.ends_ = i) : this.setCoordinates(e, t);
	}
	appendLinearRing(e) {
		this.flatCoordinates ? dt(this.flatCoordinates, e.getFlatCoordinates()) : this.flatCoordinates = e.getFlatCoordinates().slice();
		this.ends_.push(this.flatCoordinates.length);
		this.changed();
	}
	clone() {
		const e = new wn(this.flatCoordinates.slice(), this.layout, this.ends_.slice());
		e.applyProperties(this);
		return e;
	}
	closestPointXY(e, t, i, s) {
		return s < Vi(this.getExtent(), e, t) ? s : (this.maxDeltaRevision_ != this.getRevision() && (this.maxDelta_ = Math.sqrt(fl(this.flatCoordinates, 0, this.ends_, this.stride, 0)), this.maxDeltaRevision_ = this.getRevision()), _l(this.flatCoordinates, 0, this.ends_, this.stride, this.maxDelta_, true, e, t, i, s));
	}
	containsXY(e, t) {
		return rl(this.getOrientedFlatCoordinates(), 0, this.ends_, this.stride, e, t);
	}
	getArea() {
		return Ru(this.getOrientedFlatCoordinates(), 0, this.ends_, this.stride);
	}
	getCoordinates(e) {
		let t;
		e !== "undefined" ? (t = this.getOrientedFlatCoordinates().slice(), Ar(t, 0, this.ends_, this.stride, e)) : t = this.flatCoordinates;
		return xs(t, 0, this.ends_, this.stride);
	}
	getEnds() {
		return this.ends_;
	}
	getFlatInteriorPoint() {
		if (this.flatInteriorPointRevision_ != this.getRevision()) {
			const e = Ot(this.getExtent());
			this.flatInteriorPoint_ = ol(this.getOrientedFlatCoordinates(), 0, this.ends_, this.stride, e, 0);
			this.flatInteriorPointRevision_ = this.getRevision();
		}
		return this.flatInteriorPoint_;
	}
	getInteriorPoint() {
		return new pl(this.getFlatInteriorPoint(), "XYM");
	}
	getLinearRingCount() {
		return this.ends_.length;
	}
	getLinearRing(e) {
		return e < 0 || this.ends_.length <= e ? null : new Or(this.flatCoordinates.slice(e === 0 ? 0 : this.ends_[e - 1], this.ends_[e]), this.layout);
	}
	getLinearRings() {
		const e = this.layout, t = this.flatCoordinates, i = this.ends_, s = [];
		let r = 0;
		for (let o = 0, a = this.ends_.length; o < a; ++o) {
			const l = i[o];
			const h = new Or(t.slice(r, i[o]), e);
			s.push(h);
			r = i[o];
		}
		return s;
	}
	getOrientedFlatCoordinates() {
		if (this.orientedRevision_ != this.getRevision()) {
			const e = this.flatCoordinates;
			al(this.flatCoordinates, 0, this.ends_, this.stride) ? this.orientedFlatCoordinates_ = this.flatCoordinates : (this.orientedFlatCoordinates_ = this.flatCoordinates.slice(), this.orientedFlatCoordinates_.length = Ar(this.orientedFlatCoordinates_, 0, this.ends_, this.stride));
			this.orientedRevision_ = this.getRevision();
		}
		return this.orientedFlatCoordinates_;
	}
	getSimplifiedGeometryInternal(e) {
		const t = [], i = [];
		t.length = hl(this.flatCoordinates, 0, this.ends_, this.stride, Math.sqrt(e), t, 0, i);
		return new wn(t, "XY", i);
	}
	getType() {
		return "Polygon";
	}
	intersectsExtent(e) {
		return wu(this.getOrientedFlatCoordinates(), 0, this.ends_, this.stride, e);
	}
	setCoordinates(e, t) {
		this.setLayout(t, e, 2);
		if (!this.flatCoordinates) {
			this.flatCoordinates = [];
		}
		const i = Ds(this.flatCoordinates, 0, e, this.stride, this.ends_);
		this.flatCoordinates.length = i.length === 0 ? 0 : i[i.length - 1];
		this.changed();
	}
}
function Zh(n) {
	if (Pi(n)) throw new Error("Cannot create polygon from empty extent");
	const e = n[0], t = n[1], i = n[2], s = n[3], r = [
		n[0],
		n[1],
		n[0],
		n[3],
		n[2],
		n[3],
		n[2],
		n[1],
		n[0],
		n[1]
	];
	return new wn(r, "XY", [r.length]);
}
function Jg(n, e, t) {
	e = e || 32;
	const i = n.getStride(), s = n.getLayout(), r = n.getCenter(), o = i * (e + 1), a = new Array(o);
	for (let c = 0; c < o; c += i) {
		a[c] = 0;
		a[c + 1] = 0;
		for (let u = 2; u < i; u++) a[c + u] = r[u];
	}
	const l = [a.length], h = new wn(a, s, l);
	Qg(h, r, n.getRadius(), t);
	return h;
}
function Qg(n, e, t, i) {
	const s = n.getFlatCoordinates(), r = n.getStride(), o = s.length / r - 1, a = i || 0;
	for (let l = 0; l <= o; ++l) {
		const h = l * r;
		const c = a + Qt(l, o) * 2 * Math.PI / o;
		s[h] = e[0] + t * Math.cos(c);
		s[h + 1] = e[1] + t * Math.sin(c);
	}
	n.changed();
}
class Gr extends Xg {
	constructor(e, t, i) {
		super();
		this.endss_ = [];
		this.flatInteriorPointsRevision_ = -1;
		this.flatInteriorPoints_ = null;
		this.maxDelta_ = -1;
		this.maxDeltaRevision_ = -1;
		this.orientedRevision_ = -1;
		this.orientedFlatCoordinates_ = null;
		if (!i && !Array.isArray(e[0])) {
			const s = e;
			const r = [];
			const o = [];
			for (let a = 0, l = e.length; a < l; ++a) {
				const h = s[a];
				const c = r.length;
				const u = s[a].getEnds();
				for (let d = 0, f = u.length; d < f; ++d) u[d] += r.length;
				dt(r, s[a].getFlatCoordinates());
				o.push(u);
			}
			t = e.length === 0 ? this.getLayout() : e[0].getLayout();
			e = r;
			i = o;
		}
		t !== "undefined" && i ? (this.setFlatCoordinates(t, e), this.endss_ = i) : this.setCoordinates(e, t);
	}
	appendPolygon(e) {
		let t;
		if (!this.flatCoordinates) {
			this.flatCoordinates = e.getFlatCoordinates().slice();
			t = e.getEnds().slice();
			this.endss_.push();
		} else {
			const i = this.flatCoordinates.length;
			dt(this.flatCoordinates, e.getFlatCoordinates());
			t = e.getEnds().slice();
			for (let s = 0, r = t.length; s < r; ++s) t[s] += this.flatCoordinates.length;
		}
		this.endss_.push(t);
		this.changed();
	}
	clone() {
		const e = this.endss_.length, t = new Array(this.endss_.length);
		for (let s = 0; s < this.endss_.length; ++s) t[s] = this.endss_[s].slice();
		const i = new Gr(this.flatCoordinates.slice(), this.layout, t);
		i.applyProperties(this);
		return i;
	}
	closestPointXY(e, t, i, s) {
		return s < Vi(this.getExtent(), e, t) ? s : (this.maxDeltaRevision_ != this.getRevision() && (this.maxDelta_ = Math.sqrt(Zg(this.flatCoordinates, 0, this.endss_, this.stride, 0)), this.maxDeltaRevision_ = this.getRevision()), Kg(this.getOrientedFlatCoordinates(), 0, this.endss_, this.stride, this.maxDelta_, true, e, t, i, s));
	}
	containsXY(e, t) {
		return Ag(this.getOrientedFlatCoordinates(), 0, this.endss_, this.stride, e, t);
	}
	getArea() {
		return Yg(this.getOrientedFlatCoordinates(), 0, this.endss_, this.stride);
	}
	getCoordinates(e) {
		let t;
		e !== "undefined" ? (t = this.getOrientedFlatCoordinates().slice(), va(t, 0, this.endss_, this.stride, e)) : t = this.flatCoordinates;
		return Pa(t, 0, this.endss_, this.stride);
	}
	getEndss() {
		return this.endss_;
	}
	getFlatInteriorPoints() {
		if (this.flatInteriorPointsRevision_ != this.getRevision()) {
			const e = cu(this.flatCoordinates, 0, this.endss_, this.stride);
			this.flatInteriorPoints_ = uu(this.getOrientedFlatCoordinates(), 0, this.endss_, this.stride, e);
			this.flatInteriorPointsRevision_ = this.getRevision();
		}
		return this.flatInteriorPoints_;
	}
	getInteriorPoints() {
		return new yl(this.getFlatInteriorPoints().slice(), "XYM");
	}
	getOrientedFlatCoordinates() {
		if (this.orientedRevision_ != this.getRevision()) {
			const e = this.flatCoordinates;
			du(this.flatCoordinates, 0, this.endss_, this.stride) ? this.orientedFlatCoordinates_ = this.flatCoordinates : (this.orientedFlatCoordinates_ = this.flatCoordinates.slice(), this.orientedFlatCoordinates_.length = va(this.orientedFlatCoordinates_, 0, this.endss_, this.stride));
			this.orientedRevision_ = this.getRevision();
		}
		return this.orientedFlatCoordinates_;
	}
	getSimplifiedGeometryInternal(e) {
		const t = [], i = [];
		t.length = Dg(this.flatCoordinates, 0, this.endss_, this.stride, Math.sqrt(e), t, 0, i);
		return new Gr(t, "XY", i);
	}
	getPolygon(e) {
		if (e < 0 || this.endss_.length <= e) return null;
		let t;
		const r = this.endss_[e - 1];
		t = this.endss_[e - 1][this.endss_[e - 1].length - 1];
		const i = this.endss_[e].slice(), s = i[i.length - 1];
		if (t !== 0) for (let r = 0, o = i.length; this.endss_[e - 1] < o; ++r) i[this.endss_[e - 1]] -= t;
		return new wn(this.flatCoordinates.slice(t, i[i.length - 1]), this.layout, i);
	}
	getPolygons() {
		const e = this.layout, t = this.flatCoordinates, i = this.endss_, s = [];
		let r = 0;
		for (let o = 0, a = this.endss_.length; o < a; ++o) {
			const l = i[o].slice();
			const h = l[l.length - 1];
			const c = new wn(t.slice(r, l[l.length - 1]), e, l);
			s.push(c);
			r = l[l.length - 1];
		}
		return s;
	}
	getType() {
		return "MultiPolygon";
	}
	intersectsExtent(e) {
		return qg(this.getOrientedFlatCoordinates(), 0, this.endss_, this.stride, e);
	}
	setCoordinates(e, t) {
		this.setLayout(t, e, 3);
		if (!this.flatCoordinates) {
			this.flatCoordinates = [];
		}
		const i = xu(this.flatCoordinates, 0, e, this.stride, this.endss_);
		if (i.length === 0) this.flatCoordinates.length = 0;
		else {
			const s = i[i.length - 1];
			this.flatCoordinates.length = i[i.length - 1].length === 0 ? 0 : i[i.length - 1][i[i.length - 1].length - 1];
		}
		this.changed();
	}
}
const Kh = Se();
class Pn {
	constructor(e, t, i, s, r, o) {
		this.styleFunction;
		this.extent_;
		this.id_ = o;
		this.type_ = e;
		this.flatCoordinates_ = t;
		this.flatInteriorPoints_ = null;
		this.flatMidpoints_ = null;
		this.ends_ = i || null;
		this.properties_ = r;
		this.squaredTolerance_;
		this.stride_ = s;
		this.simplifiedGeometry_;
	}
	get(e) {
		return this.properties_[e];
	}
	getExtent() {
		if (!this.extent_) {
			this.extent_ = this.type_ === "Point" ? rs(this.flatCoordinates_) : Za(this.flatCoordinates_, 0, this.flatCoordinates_.length, 2);
		}
		return this.extent_;
	}
	getFlatInteriorPoint() {
		if (!this.flatInteriorPoints_) {
			const e = Ot(this.getExtent());
			this.flatInteriorPoints_ = ol(this.flatCoordinates_, 0, this.ends_, 2, e, 0);
		}
		return this.flatInteriorPoints_;
	}
	getFlatInteriorPoints() {
		if (!this.flatInteriorPoints_) {
			const e = ll(this.flatCoordinates_, this.ends_);
			const t = cu(this.flatCoordinates_, 0, e, 2);
			this.flatInteriorPoints_ = uu(this.flatCoordinates_, 0, e, 2, t);
		}
		return this.flatInteriorPoints_;
	}
	getFlatMidpoint() {
		if (!this.flatMidpoints_) {
			this.flatMidpoints_ = Lr(this.flatCoordinates_, 0, this.flatCoordinates_.length, 2, .5);
		}
		return this.flatMidpoints_;
	}
	getFlatMidpoints() {
		if (!this.flatMidpoints_) {
			this.flatMidpoints_ = [];
			const e = this.flatCoordinates_;
			let t = 0;
			const i = this.ends_;
			for (let s = 0, r = this.ends_.length; s < r; ++s) {
				const o = i[s];
				const a = Lr(e, t, i[s], 2, .5);
				dt(this.flatMidpoints_, a);
				t = i[s];
			}
		}
		return this.flatMidpoints_;
	}
	getId() {
		return this.id_;
	}
	getOrientedFlatCoordinates() {
		return this.flatCoordinates_;
	}
	getGeometry() {
		return this;
	}
	getSimplifiedGeometry(e) {
		return this;
	}
	simplifyTransformed(e, t) {
		return this;
	}
	getProperties() {
		return this.properties_;
	}
	getPropertiesInternal() {
		return this.properties_;
	}
	getStride() {
		return this.stride_;
	}
	getStyleFunction() {
		return this.styleFunction;
	}
	getType() {
		return this.type_;
	}
	transform(e) {
		e = H(e);
		const t = e.getExtent(), i = e.getWorldExtent();
		if (t && i) {
			const s = Ce(i) / Ce(t);
			gt(Kh, i[0], i[3], s, -s, 0, 0, 0);
			At(this.flatCoordinates_, 0, this.flatCoordinates_.length, 2, Kh, this.flatCoordinates_);
		}
	}
	applyTransform(e) {
		e(this.flatCoordinates_, this.flatCoordinates_, this.stride_);
	}
	clone() {
		var e;
		return new Pn(this.type_, this.flatCoordinates_.slice(), (e = this.ends_) == null ? "undefined" : e.slice(), this.stride_, Object.assign({}, this.properties_), this.id_);
	}
	getEnds() {
		return this.ends_;
	}
	enableSimplifyTransformed() {
		this.simplifyTransformed = Yc((e, t) => {
			if (e === this.squaredTolerance_) return this.simplifiedGeometry_;
			this.simplifiedGeometry_ = this.clone();
			if (t) {
				this.simplifiedGeometry_.applyTransform(t);
			}
			const i = this.simplifiedGeometry_.getFlatCoordinates();
			let s;
			switch (this.type_) {
				case "LineString":
					i.length = ho(i, 0, this.simplifiedGeometry_.flatCoordinates_.length, this.simplifiedGeometry_.stride_, e, i, 0), s = [i.length];
					break;
				case "MultiLineString":
					s = [], i.length = fu(i, 0, this.simplifiedGeometry_.ends_, this.simplifiedGeometry_.stride_, e, i, 0, s);
					break;
				case "Polygon":
					s = [], i.length = hl(i, 0, this.simplifiedGeometry_.ends_, this.simplifiedGeometry_.stride_, Math.sqrt(e), i, 0, s);
					break;
			}
			if (s) {
				this.simplifiedGeometry_ = new Pn(this.type_, i, s, 2, this.properties_, this.id_);
			}
			this.squaredTolerance_ = e;
			return this.simplifiedGeometry_;
		});
		return this;
	}
}
Pn.prototype.getFlatCoordinates = Pn.prototype.getOrientedFlatCoordinates;
function Pu(n, e, t = 0, i = n.length - 1, s = e_) {
	for (; i > t;) {
		if (i - t > 600) {
			const l = i - t + 1;
			const h = e - t + 1;
			const c = Math.log(l);
			const u = .5 * Math.exp(2 * c / 3);
			const d = .5 * Math.sqrt(c * u * (l - u) / l) * (h - l / 2 < 0 ? -1 : 1);
			const f = Math.max(t, Math.floor(e - h * u / l + d));
			const g = Math.min(i, Math.floor(e + (l - h) * u / l + d));
			Pu(n, e, f, g, s);
		}
		const r = n[e];
		let o = t;
		let a = i;
		for (Kn(n, t, e), s(n[i], n[e]) > 0 && Kn(n, t, i); o < a;) {
			for (Kn(n, o, a), o++, a--; s(n[o], r) < 0;) o++;
			for (; s(n[a], r) > 0;) a--;
		}
		s(n[t], n[e]) === 0 ? Kn(n, t, a) : (a++, Kn(n, a, i));
		if (a <= e) {
			t = a + 1;
		}
		if (e <= a) {
			i = a - 1;
		}
	}
}
function Kn(n, e, t) {
	const i = n[e];
	n[e] = n[t];
	n[t] = n[e];
}
function e_(n, e) {
	return n < e ? -1 : n > e ? 1 : 0;
}
let Iu = class {
	constructor(e = 9) {
		this._maxEntries = Math.max(4, e);
		this._minEntries = Math.max(2, Math.ceil(this._maxEntries * .4));
		this.clear();
	}
	all() {
		return this._all(this.data, []);
	}
	search(e) {
		let t = this.data;
		const i = [];
		if (!Qs(e, t)) return i;
		const s = this.toBBox, r = [];
		for (; t;) {
			for (let o = 0; o < t.children.length; o++) {
				const a = t.children[o];
				const l = t.leaf ? s(t.children[o]) : t.children[o];
				if (Qs(e, l)) {
					t.leaf ? i.push(t.children[o]) : Do(e, l) ? this._all(t.children[o], i) : r.push(t.children[o]);
				}
			}
			t = r.pop();
		}
		return i;
	}
	collides(e) {
		let t = this.data;
		if (!Qs(e, t)) return false;
		const i = [];
		for (; t;) {
			for (let s = 0; s < t.children.length; s++) {
				const r = t.children[s];
				const o = t.leaf ? this.toBBox(t.children[s]) : t.children[s];
				if (Qs(e, o)) {
					if (t.leaf || Do(e, o)) return true;
					i.push(r);
				}
			}
			t = i.pop();
		}
		return false;
	}
	load(e) {
		if (!(e && e.length)) return this;
		if (e.length < this._minEntries) {
			for (let i = 0; i < e.length; i++) this.insert(e[i]);
			return this;
		}
		let t = this._build(e.slice(), 0, e.length - 1, 0);
		if (!this.data.children.length) this.data = t;
		else if (this.data.height === t.height) this._splitRoot(this.data, t);
		else {
			if (this.data.height < t.height) {
				const i = this.data;
				this.data = t;
				t = this.data;
			}
			this._insert(t, this.data.height - t.height - 1, true);
		}
		return this;
	}
	insert(e) {
		if (e) {
			this._insert(e, this.data.height - 1);
		}
		return this;
	}
	clear() {
		this.data = cn([]);
		return this;
	}
	remove(e, t) {
		if (!e) return this;
		let i = this.data;
		const s = this.toBBox(e), r = [], o = [];
		let a, l, h;
		for (; i || r.length;) {
			if (!i) {
				i = r.pop();
				l = r[r.length - 1];
				a = o.pop();
				h = true;
			}
			if (i.leaf) {
				const c = t_(e, i.children, t);
				if (c !== -1) return i.children.splice(c, 1), r.push(i), this._condense(r), this;
			}
			!h && !i.leaf && Do(i, s) ? (r.push(i), o.push(a), a = 0, l = i, i = i.children[0]) : l ? (a++, i = l.children[a], h = false) : i = null;
		}
		return this;
	}
	toBBox(e) {
		return e;
	}
	compareMinX(e, t) {
		return e.minX - t.minX;
	}
	compareMinY(e, t) {
		return e.minY - t.minY;
	}
	toJSON() {
		return this.data;
	}
	fromJSON(e) {
		this.data = e;
		return this;
	}
	_all(e, t) {
		const i = [];
		for (; e;) {
			e.leaf ? t.push(...e.children) : i.push(...e.children);
			e = i.pop();
		}
		return t;
	}
	_build(e, t, i, s) {
		const r = i - t + 1;
		let o = this._maxEntries, a;
		if (r <= o) return a = cn(e.slice(t, i + 1)), Ji(a, this.toBBox), a;
		if (!s) {
			s = Math.ceil(Math.log(r) / Math.log(o));
			o = Math.ceil(r / Math.pow(o, s - 1));
		}
		a = cn([]);
		a.leaf = false;
		a.height = s;
		const l = Math.ceil(r / o), h = l * Math.ceil(Math.sqrt(o));
		Hh(e, t, i, h, this.compareMinX);
		for (let c = t; c <= i; c += h) {
			const u = Math.min(c + h - 1, i);
			Hh(e, c, u, l, this.compareMinY);
			for (let d = c; d <= u; d += l) {
				const f = Math.min(d + l - 1, u);
				a.children.push(this._build(e, d, f, s - 1));
			}
		}
		Ji(a, this.toBBox);
		return a;
	}
	_chooseSubtree(e, t, i, s) {
		for (; s.push(t), !(t.leaf || s.length - 1 === i);) {
			let r = null;
			let o = null;
			let a;
			for (let l = 0; l < t.children.length; l++) {
				const h = t.children[l];
				const c = Oo(t.children[l]);
				const u = s_(e, t.children[l]) - c;
				u < o ? (o = u, r = c < r ? c : r, a = t.children[l]) : u === o && c < r && (r = c, a = t.children[l]);
			}
			t = a || t.children[0];
		}
		return t;
	}
	_insert(e, t, i) {
		const s = i ? e : this.toBBox(e), r = [], o = this._chooseSubtree(s, this.data, t, r);
		for (o.children.push(e), is(o, s); t >= 0 && r[t].children.length > this._maxEntries;) {
			this._split(r, t);
			t--;
		}
		this._adjustParentBBoxes(s, r, t);
	}
	_split(e, t) {
		const i = e[t], s = e[t].children.length, r = this._minEntries;
		this._chooseSplitAxis(e[t], this._minEntries, e[t].children.length);
		const o = this._chooseSplitIndex(e[t], this._minEntries, e[t].children.length), a = cn(e[t].children.splice(o, e[t].children.length - o));
		a.height = e[t].height;
		a.leaf = e[t].leaf;
		Ji(e[t], this.toBBox);
		Ji(a, this.toBBox);
		t ? e[t - 1].children.push(a) : this._splitRoot(e[t], a);
	}
	_splitRoot(e, t) {
		this.data = cn([e, t]);
		this.data.height = e.height + 1;
		this.data.leaf = false;
		Ji(this.data, this.toBBox);
	}
	_chooseSplitIndex(e, t, i) {
		let s, r = null, o = null;
		for (let a = t; a <= i - t; a++) {
			const l = ts(e, 0, a, this.toBBox);
			const h = ts(e, a, i, this.toBBox);
			const c = r_(l, h);
			const u = Oo(l) + Oo(h);
			c < r ? (r = c, s = a, o = u < o ? u : o) : c === r && u < o && (o = u, s = a);
		}
		return s || i - t;
	}
	_chooseSplitAxis(e, t, i) {
		const s = e.leaf ? this.compareMinX : i_, r = e.leaf ? this.compareMinY : n_, o = this._allDistMargin(e, t, i, s), a = this._allDistMargin(e, t, i, r);
		if (o < a) {
			e.children.sort(s);
		}
	}
	_allDistMargin(e, t, i, s) {
		e.children.sort(s);
		const r = this.toBBox, o = ts(e, 0, t, this.toBBox), a = ts(e, i - t, i, this.toBBox);
		let l = Js(o) + Js(a);
		for (let h = t; h < i - t; h++) {
			const c = e.children[h];
			is(o, e.leaf ? r(e.children[h]) : e.children[h]);
			l += Js(o);
		}
		for (let h = i - t - 1; h >= t; h--) {
			const c = e.children[h];
			is(a, e.leaf ? r(e.children[h]) : e.children[h]);
			l += Js(a);
		}
		return l;
	}
	_adjustParentBBoxes(e, t, i) {
		for (let s = i; s >= 0; s--) is(t[s], e);
	}
	_condense(e) {
		for (let t = e.length - 1, i; t >= 0; t--) e[t].children.length === 0 ? t > 0 ? (i = e[t - 1].children, i.splice(i.indexOf(e[t]), 1)) : this.clear() : Ji(e[t], this.toBBox);
	}
};
function t_(n, e, t) {
	if (!t) return e.indexOf(n);
	for (let i = 0; i < e.length; i++) if (t(n, e[i])) return i;
	return -1;
}
function Ji(n, e) {
	ts(n, 0, n.children.length, e, n);
}
function ts(n, e, t, i, s) {
	if (!s) {
		s = cn(null);
	}
	s.minX = null;
	s.minY = null;
	s.maxX = null;
	s.maxY = null;
	for (let r = e; r < t; r++) {
		const o = n.children[r];
		is(s, n.leaf ? i(n.children[r]) : n.children[r]);
	}
	return s;
}
function is(n, e) {
	n.minX = Math.min(n.minX, e.minX);
	n.minY = Math.min(n.minY, e.minY);
	n.maxX = Math.max(n.maxX, e.maxX);
	n.maxY = Math.max(n.maxY, e.maxY);
	return n;
}
function i_(n, e) {
	return n.minX - e.minX;
}
function n_(n, e) {
	return n.minY - e.minY;
}
function Oo(n) {
	return (n.maxX - n.minX) * (n.maxY - n.minY);
}
function Js(n) {
	return n.maxX - n.minX + (n.maxY - n.minY);
}
function s_(n, e) {
	return (Math.max(e.maxX, n.maxX) - Math.min(e.minX, n.minX)) * (Math.max(e.maxY, n.maxY) - Math.min(e.minY, n.minY));
}
function r_(n, e) {
	const t = Math.max(n.minX, e.minX), i = Math.max(n.minY, e.minY), s = Math.min(n.maxX, e.maxX), r = Math.min(n.maxY, e.maxY);
	return Math.max(0, s - t) * Math.max(0, r - i);
}
function Do(n, e) {
	return n.minX <= e.minX && n.minY <= e.minY && e.maxX <= n.maxX && e.maxY <= n.maxY;
}
function Qs(n, e) {
	return e.minX <= n.maxX && e.minY <= n.maxY && e.maxX >= n.minX && e.maxY >= n.minY;
}
function cn(n) {
	return {
		children: n,
		height: 1,
		leaf: true,
		minX: null,
		minY: null,
		maxX: null,
		maxY: null
	};
}
function Hh(n, e, t, i, s) {
	const r = [e, t];
	for (; r.length;) {
		t = r.pop();
		e = r.pop();
		if (t - e <= i) continue;
		const o = e + Math.ceil((t - e) / i / 2) * i;
		Pu(n, o, e, t, s);
		r.push(e, o, o, t);
	}
}
class o_ {
	constructor(e) {
		this.rbush_ = new Iu(e);
		this.items_ = {};
	}
	insert(e, t) {
		const i = {
			minX: e[0],
			minY: e[1],
			maxX: e[2],
			maxY: e[3],
			value: t
		};
		this.rbush_.insert(i);
		this.items_[O(t)] = i;
	}
	load(e, t) {
		const i = new Array(t.length);
		for (let s = 0, r = t.length; s < r; s++) {
			const o = e[s];
			const a = t[s];
			const l = {
				minX: e[s][0],
				minY: e[s][1],
				maxX: e[s][2],
				maxY: e[s][3],
				value: t[s]
			};
			i[s] = l;
			this.items_[O(t[s])] = l;
		}
		this.rbush_.load(i);
	}
	remove(e) {
		const t = O(e), i = this.items_[t];
		delete this.items_[t];
		return this.rbush_.remove(this.items_[t]) !== null;
	}
	update(e, t) {
		const i = this.items_[O(t)], s = [
			this.items_[O(t)].minX,
			this.items_[O(t)].minY,
			this.items_[O(t)].maxX,
			this.items_[O(t)].maxY
		];
		if (!Si(s, e)) {
			this.remove(t);
			this.insert(e, t);
		}
	}
	getAll() {
		return this.rbush_.all().map(function(t) {
			return t.value;
		});
	}
	getInExtent(e) {
		const t = {
			minX: e[0],
			minY: e[1],
			maxX: e[2],
			maxY: e[3]
		};
		return this.rbush_.search(t).map(function(s) {
			return s.value;
		});
	}
	forEach(e) {
		return this.forEach_(this.getAll(), e);
	}
	forEachInExtent(e, t) {
		return this.forEach_(this.getInExtent(e), t);
	}
	forEach_(e, t) {
		let i;
		for (let s = 0, r = e.length; s < r; s++) if (i = t(e[s]), i) return i;
		return i;
	}
	isEmpty() {
		return si(this.items_);
	}
	clear() {
		this.rbush_.clear();
		this.items_ = {};
	}
	getExtent(e) {
		const t = this.rbush_.toJSON();
		return bt(t.minX, t.minY, t.maxX, t.maxY, e);
	}
	concat(e) {
		this.rbush_.load(e.rbush_.all());
		for (const t in e.items_) this.items_[t] = e.items_[t];
	}
}
class a_ extends Lf {
	constructor(e) {
		var i;
		super();
		this.projection = H(e.projection);
		this.attributions_ = qh(e.attributions);
		this.attributionsCollapsible_ = (i = e.attributionsCollapsible) != null ? i : true;
		this.loading = false;
		this.state_ = e.state !== "undefined" ? e.state : "ready";
		this.wrapX_ = e.wrapX !== "undefined" ? e.wrapX : false;
		this.interpolate_ = !!e.interpolate;
		this.viewResolver = null;
		this.viewRejector = null;
		const t = this;
		this.viewPromise_ = new Promise(function(s, r) {
			t.viewResolver = s;
			t.viewRejector = r;
		});
	}
	getAttributions() {
		return this.attributions_;
	}
	getAttributionsCollapsible() {
		return this.attributionsCollapsible_;
	}
	getProjection() {
		return this.projection;
	}
	getResolutions(e) {
		return null;
	}
	getView() {
		return this.viewPromise_;
	}
	getState() {
		return this.state_;
	}
	getWrapX() {
		return this.wrapX_;
	}
	getInterpolate() {
		return this.interpolate_;
	}
	refresh() {
		this.changed();
	}
	setAttributions(e) {
		this.attributions_ = qh(e);
		this.changed();
	}
	setState(e) {
		this.state_ = e;
		this.changed();
	}
}
function qh(n) {
	return n ? typeof n == "function" ? n : (Array.isArray(n) || (n = [n]), (e) => n) : null;
}
const Te = {
	ADDFEATURE: "addfeature",
	CHANGEFEATURE: "changefeature",
	CLEAR: "clear",
	REMOVEFEATURE: "removefeature",
	FEATURESLOADSTART: "featuresloadstart",
	FEATURESLOADEND: "featuresloadend",
	FEATURESLOADERROR: "featuresloaderror"
};
class hi extends wf {
	constructor(e, t, i) {
		super(e);
		this.feature = t;
		this.features = i;
	}
}
class l_ extends a_ {
	constructor(e) {
		e = e || {};
		super({
			attributions: e.attributions,
			interpolate: true,
			projection: "undefined",
			state: "ready",
			wrapX: e.wrapX !== "undefined" ? e.wrapX : true
		});
		this.on;
		this.once;
		this.un;
		this.loader_ = fs;
		this.format_ = e.format || null;
		this.overlaps_ = e.overlaps === "undefined" ? true : e.overlaps;
		this.url_ = e.url;
		e.loader !== "undefined" ? this.loader_ = e.loader : this.url_ !== "undefined" && (ee(this.format_, "`format` must be set when `url` is set"), this.loader_ = bh(this.url_, this.format_));
		this.strategy_ = e.strategy !== "undefined" ? e.strategy : Ig;
		const t = e.useSpatialIndex !== "undefined" ? e.useSpatialIndex : true;
		this.featuresRtree_ = t ? new o_() : null;
		this.loadedExtentsRtree_ = new o_();
		this.loadingExtentsCount_ = 0;
		this.nullGeometryFeatures_ = {};
		this.idIndex_ = {};
		this.uidIndex_ = {};
		this.featureChangeKeys_ = {};
		this.featuresCollection_ = null;
		let i, s;
		Array.isArray(e.features) ? s = e.features : e.features && (i = e.features, s = i.getArray());
		if (!t && i === "undefined") {
			i = new Af(s);
		}
		if (s !== "undefined") {
			this.addFeaturesInternal(s);
		}
		if (i !== "undefined") {
			this.bindFeaturesCollection_(i);
		}
	}
	addFeature(e) {
		this.addFeatureInternal(e);
		this.changed();
	}
	addFeatureInternal(e) {
		const t = O(e);
		if (!this.addToIndex_(t, e)) {
			if (this.featuresCollection_) {
				this.featuresCollection_.remove(e);
			}
			return;
		}
		this.setupChangeEvents_(t, e);
		const i = e.getGeometry();
		if (i) {
			const s = i.getExtent();
			if (this.featuresRtree_) {
				this.featuresRtree_.insert(s, e);
			}
		} else this.nullGeometryFeatures_[t] = e;
		this.dispatchEvent(new hi(Te.ADDFEATURE, e));
	}
	setupChangeEvents_(e, t) {
		if (!(t instanceof Pn)) {
			this.featureChangeKeys_[e] = [Z(t, U.CHANGE, this.handleFeatureChange_, this), Z(t, xt.PROPERTYCHANGE, this.handleFeatureChange_, this)];
		}
	}
	addToIndex_(e, t) {
		let i = true;
		if (t.getId() !== "undefined") {
			const s = String(t.getId());
			if (!(s in this.idIndex_)) this.idIndex_[s] = t;
			else if (t instanceof Pn) {
				const r = this.idIndex_[s];
				this.idIndex_[s] instanceof Pn ? Array.isArray(this.idIndex_[s]) ? this.idIndex_[s].push(t) : this.idIndex_[s] = [this.idIndex_[s], t] : i = false;
			} else i = false;
		}
		ee(!(e in this.uidIndex_), "The passed `feature` was already added to the source");
		this.uidIndex_[e] = t;
		return i;
	}
	addFeatures(e) {
		this.addFeaturesInternal(e);
		this.changed();
	}
	addFeaturesInternal(e) {
		const t = [], i = [], s = [];
		for (let r = 0, o = e.length; r < o; r++) {
			const a = e[r];
			const l = O(e[r]);
			if (this.addToIndex_(l, e[r])) {
				i.push(e[r]);
			}
		}
		for (let r = 0, o = i.length; r < o; r++) {
			const a = i[r];
			const l = O(i[r]);
			this.setupChangeEvents_(l, i[r]);
			const h = i[r].getGeometry();
			if (h) {
				const c = h.getExtent();
				t.push(c);
				s.push(a);
			} else this.nullGeometryFeatures_[l] = i[r];
		}
		if (this.featuresRtree_) {
			this.featuresRtree_.load(t, s);
		}
		if (this.hasListener(Te.ADDFEATURE)) for (let r = 0, o = i.length; r < o; r++) this.dispatchEvent(new hi(Te.ADDFEATURE, i[r]));
	}
	bindFeaturesCollection_(e) {
		let t = false;
		this.addEventListener(Te.ADDFEATURE, function(i) {
			t = true;
			e.push(i.feature);
			t = false;
		});
		this.addEventListener(Te.REMOVEFEATURE, function(i) {
			t = true;
			e.remove(i.feature);
			t = false;
		});
		e.addEventListener(_e.ADD, (i) => {
			t = true;
			this.addFeature(i.element);
			t = false;
		});
		e.addEventListener(_e.REMOVE, (i) => {
			t = true;
			this.removeFeature(i.element);
			t = false;
		});
		this.featuresCollection_ = e;
	}
	clear(e) {
		if (e) {
			for (const i in this.featureChangeKeys_) this.featureChangeKeys_[i].forEach(se);
			if (!this.featuresCollection_) {
				this.featureChangeKeys_ = {};
				this.idIndex_ = {};
				this.uidIndex_ = {};
			}
		} else if (this.featuresRtree_) {
			this.featuresRtree_.forEach((i) => {
				this.removeFeatureInternal(i);
			});
			for (const i in this.nullGeometryFeatures_) this.removeFeatureInternal(this.nullGeometryFeatures_[i]);
		}
		if (this.featuresCollection_) {
			this.featuresCollection_.clear();
		}
		if (this.featuresRtree_) {
			this.featuresRtree_.clear();
		}
		this.nullGeometryFeatures_ = {};
		const t = new hi(Te.CLEAR);
		this.dispatchEvent(t);
		this.changed();
	}
	forEachFeature(e) {
		if (this.featuresRtree_) return this.featuresRtree_.forEach(e);
		if (this.featuresCollection_) {
			this.featuresCollection_.forEach(e);
		}
	}
	forEachFeatureAtCoordinateDirect(e, t) {
		const i = [
			e[0],
			e[1],
			e[0],
			e[1]
		];
		return this.forEachFeatureInExtent(i, function(s) {
			const r = s.getGeometry();
			if (r instanceof Pn || r.intersectsCoordinate(e)) return t(s);
		});
	}
	forEachFeatureInExtent(e, t) {
		if (this.featuresRtree_) return this.featuresRtree_.forEachInExtent(e, t);
		if (this.featuresCollection_) {
			this.featuresCollection_.forEach(t);
		}
	}
	forEachFeatureIntersectingExtent(e, t) {
		return this.forEachFeatureInExtent(e, function(i) {
			const s = i.getGeometry();
			if (s instanceof Pn || s.intersectsExtent(e)) {
				const r = t(i);
				if (r) return r;
			}
		});
	}
	getFeaturesCollection() {
		return this.featuresCollection_;
	}
	getFeatures() {
		let e;
		this.featuresCollection_ ? e = this.featuresCollection_.getArray().slice(0) : this.featuresRtree_ && (e = this.featuresRtree_.getAll(), si(this.nullGeometryFeatures_) || dt(e, Object.values(this.nullGeometryFeatures_)));
		return e;
	}
	getFeaturesAtCoordinate(e) {
		const t = [];
		this.forEachFeatureAtCoordinateDirect(e, function(i) {
			t.push(i);
		});
		return t;
	}
	getFeaturesInExtent(e, t) {
		if (this.featuresRtree_) {
			if (!(t && t.canWrapX() && this.getWrapX())) return this.featuresRtree_.getInExtent(e);
			const s = eu(e, t);
			return [].concat(...s.map((r) => this.featuresRtree_.getInExtent(r)));
		}
		return this.featuresCollection_ ? this.featuresCollection_.getArray().slice(0) : [];
	}
	getClosestFeatureToCoordinate(e, t) {
		const i = e[0], s = e[1];
		let r = null;
		const o = [NaN, NaN];
		let a = null;
		const l = [
			null,
			null,
			null,
			null
		];
		t = t || ni;
		this.featuresRtree_.forEachInExtent(l, function(h) {
			if (t(h)) {
				const c = h.getGeometry();
				const u = a;
				a = c instanceof Pn ? 0 : c.closestPointXY(i, s, o, a);
				if (a < a) {
					r = h;
					const d = Math.sqrt(a);
					l[0] = i - d;
					l[1] = s - d;
					l[2] = i + d;
					l[3] = s + d;
				}
			}
		});
		return r;
	}
	getExtent(e) {
		var t, i;
		return (i = (t = this.featuresRtree_) == null ? "undefined" : t.getExtent(e)) != null ? i : null;
	}
	getFeatureById(e) {
		const t = this.idIndex_[e.toString()];
		return this.idIndex_[e.toString()] !== "undefined" ? this.idIndex_[e.toString()] : null;
	}
	getFeatureByUid(e) {
		const t = this.uidIndex_[e];
		return this.uidIndex_[e] !== "undefined" ? this.uidIndex_[e] : null;
	}
	getFormat() {
		return this.format_;
	}
	getOverlaps() {
		return this.overlaps_;
	}
	getUrl() {
		return this.url_;
	}
	handleFeatureChange_(e) {
		const t = e.target, i = O(e.target), s = e.target.getGeometry();
		if (!s) i in this.nullGeometryFeatures_ || (this.featuresRtree_ && this.featuresRtree_.remove(e.target), this.nullGeometryFeatures_[i] = e.target);
		else {
			const o = s.getExtent();
			i in this.nullGeometryFeatures_ ? (delete this.nullGeometryFeatures_[i], this.featuresRtree_ && this.featuresRtree_.insert(o, t)) : this.featuresRtree_ && this.featuresRtree_.update(o, t);
		}
		const r = e.target.getId();
		if (r !== "undefined") {
			const o = r.toString();
			if (this.idIndex_[o] !== t) {
				this.removeFromIdIndex_(t);
				this.idIndex_[o] = t;
			}
		} else {
			this.removeFromIdIndex_(e.target);
			this.uidIndex_[i] = e.target;
		}
		this.changed();
		this.dispatchEvent(new hi(Te.CHANGEFEATURE, e.target));
	}
	hasFeature(e) {
		const t = e.getId();
		return t !== "undefined" ? t in this.idIndex_ : O(e) in this.uidIndex_;
	}
	isEmpty() {
		return this.featuresRtree_ ? this.featuresRtree_.isEmpty() && si(this.nullGeometryFeatures_) : this.featuresCollection_ ? this.featuresCollection_.getLength() === 0 : true;
	}
	loadFeatures(e, t, i) {
		const s = this.loadedExtentsRtree_, r = this.strategy_(e, t, i);
		for (let o = 0, a = r.length; o < a; ++o) {
			const l = r[o];
			if (!s.forEachInExtent(r[o], function(c) {
				return at(c.extent, l);
			})) {
				++this.loadingExtentsCount_;
				this.dispatchEvent(new hi(Te.FEATURESLOADSTART));
				this.loader_(r[o], t, i, (c) => {
					--this.loadingExtentsCount_;
					this.dispatchEvent(new hi(Te.FEATURESLOADEND, "undefined", c));
				}, () => {
					--this.loadingExtentsCount_;
					this.dispatchEvent(new hi(Te.FEATURESLOADERROR));
				});
				s.insert(r[o], { extent: r[o].slice() });
			}
		}
		this.loading = this.loader_.length < 4 ? false : this.loadingExtentsCount_ > 0;
	}
	refresh() {
		this.clear(true);
		this.loadedExtentsRtree_.clear();
		super.refresh();
	}
	removeLoadedExtent(e) {
		const t = this.loadedExtentsRtree_, i = this.loadedExtentsRtree_.forEachInExtent(e, function(s) {
			if (Si(s.extent, e)) return s;
		});
		if (i) {
			this.loadedExtentsRtree_.remove(i);
		}
	}
	removeFeatures(e) {
		let t = false;
		for (let i = 0, s = e.length; i < s; ++i) t = this.removeFeatureInternal(e[i]) || t;
	}
	removeFeature(e) {
		if (!e) return;
		if (this.removeFeatureInternal(e)) {
			this.changed();
		}
	}
	removeFeatureInternal(e) {
		const t = O(e);
		if (!(t in this.uidIndex_)) return false;
		t in this.nullGeometryFeatures_ ? delete this.nullGeometryFeatures_[t] : this.featuresRtree_ && this.featuresRtree_.remove(e);
		const i = this.featureChangeKeys_[t];
		if (!(this.featureChangeKeys_[t] == null)) {
			this.featureChangeKeys_[t].forEach(se);
		}
		delete this.featureChangeKeys_[t];
		const s = e.getId();
		if (s !== "undefined") {
			const r = s.toString();
			const o = this.idIndex_[r];
			this.idIndex_[r] === e ? delete this.idIndex_[r] : Array.isArray(this.idIndex_[r]) && (this.idIndex_[r].splice(this.idIndex_[r].indexOf(e), 1), this.idIndex_[r].length === 1 && (this.idIndex_[r] = this.idIndex_[r][0]));
		}
		delete this.uidIndex_[t];
		if (this.hasListener(Te.REMOVEFEATURE)) {
			this.dispatchEvent(new hi(Te.REMOVEFEATURE, e));
		}
		return true;
	}
	removeFromIdIndex_(e) {
		for (const t in this.idIndex_) if (this.idIndex_[t] === e) {
			delete this.idIndex_[t];
			break;
		}
	}
	setLoader(e) {
		this.loader_ = e;
	}
	setUrl(e) {
		ee(this.format_, "`format` must be set when `url` is set");
		this.url_ = e;
		this.setLoader(bh(e, this.format_));
	}
	setOverlaps(e) {
		this.overlaps_ = e;
		this.changed();
	}
}
const Me = {
	PRERENDER: "prerender",
	POSTRENDER: "postrender",
	PRECOMPOSE: "precompose",
	POSTCOMPOSE: "postcompose",
	RENDERCOMPLETE: "rendercomplete"
};
const zi = typeof navigator < "u" && typeof navigator.userAgent < "u" ? navigator.userAgent.toLowerCase() : "";
const h_ = zi.includes("safari") && !zi.includes("chrom");
const c_ = h_ && (zi.includes("version/15.4") || /cpu (os|iphone os) 15_4 like mac os x/.test(zi));
const u_ = zi.includes("webkit") && !zi.includes("edge");
const El = zi.includes("macintosh");
const Lu = typeof devicePixelRatio < "u" ? devicePixelRatio : 1;
const ht = typeof WorkerGlobalScope < "u" && typeof OffscreenCanvas < "u" && self instanceof WorkerGlobalScope;
const Tl = typeof Image < "u" && Image.prototype.decode;
const Au = typeof createImageBitmap == "function";
const Mu = false;
const $ = {
	IDLE: 0,
	LOADING: 1,
	LOADED: 2,
	ERROR: 3,
	EMPTY: 4
};
function Re(n, e, t, i) {
	let s;
	t && t.length ? s = t.shift() : ht ? s = new class extends OffscreenCanvas {
		constructor() {
			super(...arguments);
			Fo(this, "style", {});
		}
	}(n != null ? n : 300, e != null ? e : 150) : s = document.createElement("canvas");
	if (n) {
		s.width = n;
	}
	if (e) {
		s.height = e;
	}
	return s.getContext("2d", i);
}
let No;
function $r() {
	if (!No) {
		No = Re(1, 1);
	}
	return No;
}
function Ns(n) {
	const e = n.canvas;
	n.canvas.width = 1;
	n.canvas.height = 1;
	n.clearRect(0, 0, 1, 1);
}
function d_(n) {
	let e = n.offsetWidth;
	const t = getComputedStyle(n);
	e += parseInt(t.marginLeft, 10) + parseInt(t.marginRight, 10);
	return e;
}
function f_(n) {
	let e = n.offsetHeight;
	const t = getComputedStyle(n);
	e += parseInt(t.marginTop, 10) + parseInt(t.marginBottom, 10);
	return e;
}
function Jh(n, e) {
	const t = e.parentNode;
	if (e.parentNode) {
		e.parentNode.replaceChild(n, e);
	}
}
function bu(n) {
	for (; n.lastChild;) n.lastChild.remove();
}
function g_(n, e) {
	const t = n.childNodes;
	for (let i = 0;; ++i) {
		const s = t[i];
		const r = e[i];
		if (!t[i] && !e[i]) break;
		if (t[i] !== e[i]) {
			if (!s) {
				n.appendChild(r);
				continue;
			}
			if (!r) {
				n.removeChild(s);
				--i;
				continue;
			}
			n.insertBefore(r, s);
		}
	}
}
function Ou() {
	return new Proxy({
		childNodes: [],
		appendChild: function(e) {
			this.childNodes.push(e);
			return e;
		},
		remove: function() {},
		removeChild: function(e) {
			const t = this.childNodes.indexOf(e);
			if (t === -1) throw new Error("Node to remove was not found");
			this.childNodes.splice(t, 1);
			return e;
		},
		insertBefore: function(e, t) {
			const i = this.childNodes.indexOf(t);
			if (i === -1) throw new Error("Reference node not found");
			this.childNodes.splice(i, 0, e);
			return e;
		},
		style: {}
	}, { get(e, t, i) {
		return t === "firstElementChild" ? e.childNodes.length > 0 ? e.childNodes[0] : null : Reflect.get(e, t, i);
	} });
}
function pi(n) {
	return typeof HTMLCanvasElement < "u" && n instanceof HTMLCanvasElement || typeof OffscreenCanvas < "u" && n instanceof OffscreenCanvas;
}
const Cl = [
	NaN,
	NaN,
	NaN,
	0
];
let ko;
function __() {
	if (!ko) {
		ko = Re(1, 1, "undefined", {
			willReadFrequently: true,
			desynchronized: true
		});
	}
	return ko;
}
const m_ = /^rgba?\(\s*(\d+%?)\s+(\d+%?)\s+(\d+%?)(?:\s*\/\s*(\d+%|\d*\.\d+|[01]))?\s*\)$/i;
const p_ = /^rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)(?:\s*,\s*(\d+%|\d*\.\d+|[01]))?\s*\)$/i;
const y_ = /^rgba?\(\s*(\d+%)\s*,\s*(\d+%)\s*,\s*(\d+%)(?:\s*,\s*(\d+%|\d*\.\d+|[01]))?\s*\)$/i;
const x_ = /^#([\da-f]{3,4}|[\da-f]{6}|[\da-f]{8})$/i;
function er(n, e) {
	return n.endsWith("%") ? Number(n.substring(0, n.length - 1)) / e : Number(n);
}
function ls(n) {
	throw new Error("failed to parse \"" + n + "\" as color");
}
function Du(n) {
	if (n.toLowerCase().startsWith("rgb")) {
		const r = n.match(p_) || n.match(m_) || n.match(y_);
		if (r) {
			const o = r[4];
			const a = .39215686274509803;
			return [
				fe(er(r[1], .39215686274509803) + .5 | 0, 0, 255),
				fe(er(r[2], .39215686274509803) + .5 | 0, 0, 255),
				fe(er(r[3], .39215686274509803) + .5 | 0, 0, 255),
				r[4] !== "undefined" ? fe(er(r[4], 100), 0, 1) : 1
			];
		}
		ls(n);
	}
	if (n.startsWith("#")) {
		if (x_.test(n)) {
			const r = n.substring(1);
			const o = r.length <= 4 ? 1 : 2;
			const a = [
				0,
				0,
				0,
				255
			];
			for (let l = 0, h = r.length; l < h; l += o) {
				let c = parseInt(r.substring(l, l + o), 16);
				if (o === 1) {
					c += c << 4;
				}
				a[l / o] = c;
			}
			a[3] = a[3] / 255;
			return a;
		}
		ls(n);
	}
	const e = __();
	e.fillStyle = "#abcdef";
	let t = e.fillStyle;
	e.fillStyle = n;
	if (e.fillStyle === t) {
		e.fillStyle = "#fedcba";
		t = e.fillStyle;
		e.fillStyle = n;
		if (e.fillStyle === t) {
			ls(n);
		}
	}
	const i = e.fillStyle;
	if (e.fillStyle.startsWith("#") || e.fillStyle.startsWith("rgba")) return Du(e.fillStyle);
	e.clearRect(0, 0, 1, 1);
	e.fillRect(0, 0, 1, 1);
	const s = Array.from(e.getImageData(0, 0, 1, 1).data);
	s[3] = bn(s[3] / 255, 3);
	return s;
}
function E_(n) {
	return typeof n == "string" ? n : Sl(n);
}
const T_ = 1024;
const Hn = {};
let Go = 0;
function C_(n) {
	if (n.length === 4) return n;
	const e = n.slice();
	e[3] = 1;
	return e;
}
function Bo(n) {
	return n > .0031308 ? Math.pow(n, .4166666666666667) * 269.025 - 14.025 : n * 3294.6;
}
function $o(n) {
	return n > .2068965 ? Math.pow(n, 3) : (n - .13793103448275862) * .12841854934601665;
}
function Uo(n) {
	return n > 10.314724 ? Math.pow((n + 14.025) / 269.025, 2.4) : n / 3294.6;
}
function jo(n) {
	return n > .0088564 ? Math.pow(n, .3333333333333333) : n / .12841854934601665 + .13793103448275862;
}
function Qh(n) {
	const e = Uo(n[0]), t = Uo(n[1]), i = Uo(n[2]), s = jo(e * .222488403 + t * .716873169 + i * .06060791), r = 500 * (jo(e * .452247074 + t * .399439023 + i * .148375274) - s), o = 200 * (s - jo(e * .016863605 + t * .117638439 + i * .865350722)), a = Math.atan2(o, r) * (180 / Math.PI);
	return [
		116 * s - 16,
		Math.sqrt(r * r + o * o),
		a < 0 ? a + 360 : a,
		n[3]
	];
}
function R_(n) {
	const e = (n[0] + 16) / 116, t = n[1], i = n[2] * Math.PI / 180, s = $o(e), r = $o(e + n[1] / 500 * Math.cos(i)), o = $o(e - n[1] / 200 * Math.sin(i)), a = Bo(r * 3.021973625 - s * 1.617392459 - o * .404875592), l = Bo(r * -.943766287 + s * 1.916279586 + o * .027607165), h = Bo(r * .069407491 - s * .22898585 + o * 1.159737864);
	return [
		fe(a + .5 | 0, 0, 255),
		fe(l + .5 | 0, 0, 255),
		fe(h + .5 | 0, 0, 255),
		n[3]
	];
}
function Rl(n) {
	if (Hn.hasOwnProperty(n)) return Hn[n];
	if (Go >= T_) {
		let t = 0;
		for (const i in Hn) t++ & 3 || (delete Hn[i], --Go);
	}
	const e = Du(n);
	if (e.length !== 4) {
		ls(n);
	}
	for (const t of e) isNaN(t) && ls(n);
	Hn[n] = e;
	++Go;
	return e;
}
function _t(n) {
	return Array.isArray(n) ? n : Rl(n);
}
function Sl(n) {
	let e = n[0];
	if (e != (e | 0)) {
		e = e + .5 | 0;
	}
	let t = n[1];
	if (t != (t | 0)) {
		t = t + .5 | 0;
	}
	let i = n[2];
	if (i != (i | 0)) {
		i = i + .5 | 0;
	}
	const s = n[3] === "undefined" ? 1 : Math.round(n[3] * 1e3) / 1e3;
	return "rgba(" + e + "," + t + "," + i + "," + s + ")";
}
class S_ extends Pf {
	constructor(e, t, i, s) {
		super();
		this.extent = e;
		this.pixelRatio_ = i;
		this.resolution = t;
		this.state = typeof s == "function" ? $.IDLE : s;
		this.image_ = null;
		this.loader = typeof s == "function" ? s : null;
	}
	changed() {
		this.dispatchEvent(U.CHANGE);
	}
	getExtent() {
		return this.extent;
	}
	getImage() {
		return this.image_;
	}
	getPixelRatio() {
		return this.pixelRatio_;
	}
	getResolution() {
		return this.resolution;
	}
	getState() {
		return this.state;
	}
	load() {
		if (this.state == $.IDLE && this.loader) {
			this.state = $.LOADING;
			this.changed();
			const e = this.getResolution();
			const t = Array.isArray(e) ? e[0] : e;
			Zc(() => this.loader(this.getExtent(), t, this.getPixelRatio())).then((i) => {
				if ("image" in i) {
					this.image_ = i.image;
				}
				if ("extent" in i) {
					this.extent = i.extent;
				}
				if ("resolution" in i) {
					this.resolution = i.resolution;
				}
				if ("pixelRatio" in i) {
					this.pixelRatio_ = i.pixelRatio;
				}
				if (i instanceof HTMLImageElement || Au && i instanceof ImageBitmap || i instanceof HTMLCanvasElement || i instanceof HTMLVideoElement) {
					this.image_ = i;
				}
				this.state = $.LOADED;
			}).catch((i) => {
				this.state = $.ERROR;
				console.error(i);
			}).finally(() => this.changed());
		}
	}
	setImage(e) {
		this.image_ = e;
	}
	setResolution(e) {
		this.resolution = e;
	}
}
function v_(n, e, t) {
	const i = n;
	let s = true, r = false, o = false;
	const a = [wr(n, U.LOAD, function() {
		o = true;
		e();
	})];
	n.src && Tl ? (r = true, n.decode().then(function() {
		e();
	}).catch(function(l) {
		o ? e() : t();
	})) : a.push(wr(n, U.ERROR, t));
	return function() {
		s = false;
		a.forEach(se);
	};
}
function w_(n, e) {
	return new Promise((t, i) => {
		function s() {
			o();
			t(n);
		}
		function r() {
			o();
			i(new Error("Image load error"));
		}
		function o() {
			n.removeEventListener("load", s);
			n.removeEventListener("error", r);
		}
		n.addEventListener("load", s);
		n.addEventListener("error", r);
		if (e) {
			n.src = e;
		}
	});
}
function Nu(n, e) {
	if (e) {
		n.src = e;
	}
	return n.src && Tl ? new Promise((t, i) => n.decode().then(() => t(n)).catch((s) => n.complete && n.width ? t(n) : i(s))) : w_(n);
}
function Bn(n, e) {
	if (e) {
		n.src = e;
	}
	return n.src && Tl && Au ? n.decode().then(() => createImageBitmap(n)).catch((t) => {
		if (n.complete && n.width) return n;
		throw t;
	}) : Nu(n);
}
class P_ {
	constructor() {
		this.cache_ = {};
		this.patternCache_ = {};
		this.cacheSize_ = 0;
		this.maxCacheSize_ = 1024;
	}
	clear() {
		this.cache_ = {};
		this.patternCache_ = {};
		this.cacheSize_ = 0;
	}
	canExpireCache() {
		return this.cacheSize_ > this.maxCacheSize_;
	}
	expire() {
		if (this.canExpireCache()) {
			let e = 0;
			for (const t in this.cache_) {
				const i = this.cache_[t];
				if (!(e++ & 3) && !this.cache_[t].hasListener()) {
					delete this.cache_[t];
					delete this.patternCache_[t];
					--this.cacheSize_;
				}
			}
		}
	}
	get(e, t) {
		const i = zo(e, t);
		return i in this.cache_ ? this.cache_[i] : null;
	}
	getPattern(e, t) {
		const i = zo(e, t);
		return i in this.patternCache_ ? this.patternCache_[i] : null;
	}
	set(e, t, i, s) {
		const r = zo(e, t), o = r in this.cache_;
		this.cache_[r] = i;
		if (s) {
			if (i.getImageState() === $.IDLE) {
				i.load();
			}
			i.getImageState() === $.LOADING ? i.ready().then(() => {
				this.patternCache_[r] = $r().createPattern(i.getImage(1), "repeat");
			}) : this.patternCache_[r] = $r().createPattern(i.getImage(1), "repeat");
		}
		if (!o) {
			++this.cacheSize_;
		}
	}
	setSize(e) {
		this.maxCacheSize_ = e;
		this.expire();
	}
}
function zo(n, e) {
	const t = e ? _t(e) : "null";
	return n + ":" + t;
}
const ct = new P_();
let qn = null;
class ku extends Pf {
	constructor(e, t, i, s, r) {
		super();
		this.hitDetectionImage_ = null;
		this.image_ = e;
		this.crossOrigin_ = i == null ? "undefined" : i.crossOrigin;
		this.referrerPolicy_ = i == null ? "undefined" : i.referrerPolicy;
		this.canvas_ = {};
		this.color_ = r;
		this.imageState_ = s === "undefined" ? $.IDLE : s;
		this.size_ = e && e.width && e.height ? [e.width, e.height] : null;
		this.src_ = t;
		this.tainted_;
		this.ready_ = null;
	}
	initializeImage_() {
		this.image_ = new Image();
		if (this.crossOrigin_ !== null) {
			this.image_.crossOrigin = this.crossOrigin_;
		}
		if (this.referrerPolicy_ !== "undefined") {
			this.image_.referrerPolicy = this.referrerPolicy_;
		}
	}
	isTainted_() {
		if (this.tainted_ === "undefined" && this.imageState_ === $.LOADED) {
			qn = Re(1, 1, "undefined", { willReadFrequently: true });
			qn.drawImage(this.image_, 0, 0);
			try {
				qn.getImageData(0, 0, 1, 1);
				this.tainted_ = false;
			} catch (e) {
				qn = null;
				this.tainted_ = true;
			}
		}
		return this.tainted_ === true;
	}
	dispatchChangeEvent_() {
		this.dispatchEvent(U.CHANGE);
	}
	handleImageError_() {
		this.imageState_ = $.ERROR;
		this.dispatchChangeEvent_();
	}
	handleImageLoad_() {
		this.imageState_ = $.LOADED;
		this.size_ = [this.image_.width, this.image_.height];
		this.dispatchChangeEvent_();
	}
	getImage(e) {
		if (!this.image_) {
			this.initializeImage_();
		}
		this.replaceColor_(e);
		return this.canvas_[e] ? this.canvas_[e] : this.image_;
	}
	setImage(e) {
		this.image_ = e;
	}
	getPixelRatio(e) {
		this.replaceColor_(e);
		return this.canvas_[e] ? e : 1;
	}
	getImageState() {
		return this.imageState_;
	}
	getHitDetectionImage() {
		if (!this.image_) {
			this.initializeImage_();
		}
		if (!this.hitDetectionImage_) if (this.isTainted_()) {
			const e = this.size_[0];
			const t = this.size_[1];
			const i = Re(this.size_[0], this.size_[1]);
			i.fillRect(0, 0, this.size_[0], this.size_[1]);
			this.hitDetectionImage_ = i.canvas;
		} else this.hitDetectionImage_ = this.image_;
		return this.hitDetectionImage_;
	}
	getSize() {
		return this.size_;
	}
	getSrc() {
		return this.src_;
	}
	load() {
		if (this.imageState_ === $.IDLE) {
			if (!this.image_) {
				this.initializeImage_();
			}
			this.imageState_ = $.LOADING;
			try {
				if (this.src_ !== "undefined") {
					this.image_.src = this.src_;
				}
			} catch (e) {
				this.handleImageError_();
			}
			if (this.image_ instanceof HTMLImageElement) {
				Nu(this.image_, this.src_).then((e) => {
					this.image_ = e;
					this.handleImageLoad_();
				}).catch(this.handleImageError_.bind(this));
			}
		}
	}
	replaceColor_(e) {
		if (!this.color_ || this.canvas_[e] || this.imageState_ !== $.LOADED) return;
		const t = this.image_, i = Re(Math.ceil(this.image_.width * e), Math.ceil(this.image_.height * e)), s = i.canvas;
		i.scale(e, e);
		i.drawImage(this.image_, 0, 0);
		i.globalCompositeOperation = "multiply";
		i.fillStyle = E_(this.color_);
		i.fillRect(0, 0, i.canvas.width / e, i.canvas.height / e);
		i.globalCompositeOperation = "destination-in";
		i.drawImage(this.image_, 0, 0);
		this.canvas_[e] = i.canvas;
	}
	ready() {
		if (!this.ready_) {
			this.ready_ = new Promise((e) => {
				if (this.imageState_ === $.LOADED || this.imageState_ === $.ERROR) e();
				else {
					const t = () => {
						if (this.imageState_ === $.LOADED || this.imageState_ === $.ERROR) {
							this.removeEventListener(U.CHANGE, t);
							e();
						}
					};
					this.addEventListener(U.CHANGE, t);
				}
			});
		}
		return this.ready_;
	}
}
function hs(n, e, t, i, s, r) {
	let o = e === "undefined" ? "undefined" : ct.get(e, s);
	if (!o) {
		o = new ku(n, n && "src" in n ? n.src || "undefined" : e, t, i, s);
		ct.set(e, s, o, r);
	}
	if (r && o && !ct.getPattern(e, s)) {
		ct.set(e, s, o, r);
	}
	return o;
}
function It(n) {
	return n ? Array.isArray(n) ? Sl(n) : typeof n == "object" && "src" in n ? I_(n) : n : null;
}
function I_(n) {
	if (!n.offset || !n.size) return ct.getPattern(n.src, n.color);
	const e = n.src + ":" + n.offset, t = ct.getPattern(e, n.color);
	if (t) return t;
	const i = ct.get(n.src, null);
	if (i.getImageState() !== $.LOADED) return null;
	const s = Re(n.size[0], n.size[1]);
	s.drawImage(i.getImage(1), n.offset[0], n.offset[1], n.size[0], n.size[1], 0, 0, n.size[0], n.size[1]);
	hs(s.canvas, e, "undefined", $.LOADED, n.color, true);
	return ct.getPattern(e, n.color);
}
function F_(n, e, t, i, s, r) {
	s = s != null ? s : [];
	r = r != null ? r : e;
	const o = n[0], a = n[1], l = n[n.length - 4], h = n[n.length - 3];
	let c, u, d, f, g, m, _, p, y = 0;
	for (let E = 0; E < n.length; E += e) {
		d = c;
		f = u;
		g = "undefined";
		m = "undefined";
		if (E + e < n.length) {
			g = n[E + e];
			m = n[E + e + 1];
		}
		if (i && E === 0) {
			d = l;
			f = h;
		}
		if (i && E === n.length - 2) {
			g = o;
			m = a;
		}
		c = n[E];
		u = n[E + 1];
		[_, p] = Tr(c, u, d, f, g, m, t);
		s[y++] = _;
		s[y++] = p;
		for (let x = 2; x < r; x++) s[y++] = n[E + x];
	}
	if (s.length != y) {
		s.length = y;
	}
	return s;
}
function Tr(n, e, t, i, s, r, o) {
	let a, l;
	t !== "undefined" && i !== "undefined" ? (a = n - t, l = e - i) : s !== "undefined" && r !== "undefined" ? (a = s - n, l = r - e) : (a = 1, l = 0);
	const h = Math.hypot(a, l), c = a / h, u = l / h;
	a = -u;
	l = c;
	if (t === "undefined" || i === "undefined") return [n + a * o, e + l * o];
	if (s === "undefined" || r === "undefined") return [n + a * o, e + l * o];
	const d = Yf([n, e], [t, i], [s, r]);
	if (Math.cos(d) > .998) return [n + c * o, e + u * o];
	const f = Math.cos(d / 2), g = Math.sin(d / 2), m = g * a + f * l, _ = -f * a + g * l, p = m * (1 / g), y = _ * (1 / g);
	return [n + p * o, e + y * o];
}
class L_ {
	drawCustom(e, t, i, s, r) {}
	drawGeometry(e) {}
	setStyle(e) {}
	drawCircle(e, t, i) {}
	drawFeature(e, t, i) {}
	drawGeometryCollection(e, t, i) {}
	drawLineString(e, t, i) {}
	drawMultiLineString(e, t, i) {}
	drawMultiPoint(e, t, i) {}
	drawMultiPolygon(e, t, i) {}
	drawPoint(e, t, i) {}
	drawPolygon(e, t, i) {}
	drawText(e, t, i) {}
	setFillStrokeStyle(e, t) {}
	setImageStyle(e, t) {}
	setTextStyle(e, t) {}
}
const tr = "ol-hidden";
const A_ = "ol-selectable";
const fo = "ol-unselectable";
const wl = "ol-control";
const ec = "ol-collapsed";
const M_ = new RegExp("^\\s*(?=(?:(?:[-a-z]+\\s*){0,2}(italic|oblique))?)(?=(?:(?:[-a-z]+\\s*){0,2}(small-caps))?)(?=(?:(?:[-a-z]+\\s*){0,2}(bold(?:er)?|lighter|[1-9]00 ))?)(?:(?:normal|\\1|\\2|\\3)\\s*){0,3}((?:xx?-)?(?:small|large)|medium|smaller|larger|[\\.\\d]+(?:\\%|in|[cem]m|ex|p[ctx]))(?:\\s*\\/\\s*(normal|[\\.\\d]+(?:\\%|in|[cem]m|ex|p[ctx])?))?\\s*([-,\\\"\\'\\sa-z0-9]+?)\\s*$", "i");
const tc = [
	"style",
	"variant",
	"weight",
	"size",
	"lineHeight",
	"family"
];
const Ia = {
	normal: 400,
	bold: 700
};
const Fa = function(n) {
	const e = n.match(M_);
	if (!e) return null;
	const t = {
		lineHeight: "normal",
		size: "1.2em",
		style: "normal",
		weight: "400",
		variant: "normal"
	};
	for (let i = 0, s = tc.length; i < s; ++i) {
		const r = e[i + 1];
		if (e[i + 1] !== "undefined") {
			t[tc[i]] = typeof e[i + 1] == "string" ? e[i + 1].trim() : e[i + 1];
		}
	}
	if (isNaN(Number(t.weight)) && t.weight in Ia) {
		t.weight = Ia[t.weight];
	}
	t.families = t.family.split(/,\s?/).map((i) => i.trim().replace(/^['"]|['"]$/g, ""));
	return t;
};
const Bu = "10px sans-serif";
const Ve = "#000";
const In = "round";
const ti = [];
const ii = 0;
const Fn = "round";
const Es = 10;
const b_ = 0;
const Ts = "#000";
const Cs = "center";
const Ur = "middle";
const Bi = [
	0,
	0,
	0,
	0
];
const Rs = 1;
const un = new Lf();
let Jn = null;
let ic;
const La = {};
const O_ = new Set([
	"serif",
	"sans-serif",
	"monospace",
	"cursive",
	"fantasy",
	"system-ui",
	"ui-serif",
	"ui-sans-serif",
	"ui-monospace",
	"ui-rounded",
	"emoji",
	"math",
	"fangsong"
]);
function D_(n, e, t) {
	return "".concat(n, " ").concat(e, " 16px \"").concat(t, "\"");
}
const N_ = function() {
	let e, t;
	async function i(r) {
		await t.ready;
		const o = await t.load(r);
		if (o.length === 0) return false;
		const a = Fa(r), l = a.families[0].toLowerCase(), h = a.weight;
		return o.some((c) => {
			const u = c.family.replace(/^['"]|['"]$/g, "").toLowerCase(), d = Ia[c.weight] || c.weight;
			return u === l && c.style === a.style && d == h;
		});
	}
	async function s() {
		await t.ready;
		let r = true;
		const o = un.getProperties(), a = Object.keys(o).filter((l) => o[l] < 100);
		for (let l = a.length - 1; l >= 0; --l) {
			const h = a[l];
			let c = o[a[l]];
			if (c < 100) {
				await i(a[l]) ? (Wi(La), un.set(a[l], 100)) : (c += 10, un.set(a[l], c, true), c < 100 && (r = false));
			}
		}
		e = "undefined";
	}
	return async function(r) {
		if (!t) {
			t = ht ? self.fonts : document.fonts;
		}
		const o = Fa(r);
		if (!o) return;
		const a = o.families;
		let l = false;
		for (const h of o.families) {
			if (O_.has(h)) continue;
			const c = D_(o.style, o.weight, h);
			if (un.get(c) === "undefined") {
				un.set(c, 0, true);
				l = true;
			}
		}
	};
}();
const k_ = function() {
	let n;
	return function(e) {
		let t = La[e];
		if (t == null) {
			if (ht) {
				const i = Fa(e);
				const s = $u(e, "Žg");
				t = (isNaN(Number(i.lineHeight)) ? 1.2 : Number(i.lineHeight)) * (s.actualBoundingBoxAscent + s.actualBoundingBoxDescent);
			} else {
				if (!n) {
					n = document.createElement("div");
					n.innerHTML = "M";
					n.style.minHeight = "0";
					n.style.maxHeight = "none";
					n.style.height = "auto";
					n.style.padding = "0";
					n.style.border = "none";
					n.style.position = "absolute";
					n.style.display = "block";
					n.style.left = "-99999px";
				}
				n.style.font = e;
				document.body.appendChild(n);
				t = n.offsetHeight;
				document.body.removeChild(n);
			}
			La[e] = t;
		}
		return t;
	};
}();
function $u(n, e) {
	Jn = Re(1, 1);
	if (n != ic) {
		Jn.font = n;
		ic = Jn.font;
	}
	return Jn.measureText(e);
}
function Uu(n, e) {
	return $u(n, e).width;
}
function nc(n, e, t) {
	if (e in t) return t[e];
	const i = e.split("\n").reduce((s, r) => Math.max(s, Uu(n, r)), 0);
	t[e] = i;
	return i;
}
function G_(n, e) {
	const t = [], i = [], s = [];
	let r = 0, o = 0, a = 0, l = 0;
	for (let h = 0, c = e.length; h <= c; h += 2) {
		const u = e[h];
		if (e[h] === "\n" || h === c) {
			r = Math.max(r, o);
			s.push(o);
			o = 0;
			a += l;
			l = 0;
			continue;
		}
		const d = e[h + 1] || n.font;
		const f = Uu(d, e[h]);
		t.push(f);
		o += f;
		const g = k_(d);
		i.push(g);
		l = Math.max(l, g);
	}
	return {
		width: r,
		height: a,
		widths: t,
		heights: i,
		lineWidths: s
	};
}
function B_(n, e, t, i, s, r, o, a, l, h, c) {
	n.save();
	n.globalAlpha === "undefined" ? n.globalAlpha = (u) => u.globalAlpha *= t : n.globalAlpha *= t;
	if (e) {
		n.transform.apply(n, e);
	}
	i.contextInstructions ? (n.translate(l, h), n.scale(c[0], c[1]), $_(i, n)) : c[0] < 0 || c[1] < 0 ? (n.translate(l, h), n.scale(c[0], c[1]), n.drawImage(i, s, r, o, a, 0, 0, o, a)) : n.drawImage(i, s, r, o, a, l, h, o * c[0], a * c[1]);
	n.restore();
}
function $_(n, e) {
	const t = n.contextInstructions;
	for (let i = 0, s = n.contextInstructions.length; i < s; i += 2) Array.isArray(n.contextInstructions[i + 1]) ? e[n.contextInstructions[i]].apply(e, n.contextInstructions[i + 1]) : e[n.contextInstructions[i]] = n.contextInstructions[i + 1];
}
class U_ extends L_ {
	constructor(e, t, i, s, r, o, a) {
		super();
		this.context_ = e;
		this.pixelRatio_ = t;
		this.extent_ = i;
		this.transform_ = s;
		this.transformRotation_ = s ? bn(Math.atan2(s[1], s[0]), 10) : 0;
		this.viewRotation_ = r;
		this.squaredTolerance_ = o;
		this.userTransform_ = a;
		this.contextFillState_ = null;
		this.contextStrokeState_ = null;
		this.contextTextState_ = null;
		this.fillState_ = null;
		this.strokeState_ = null;
		this.image_ = null;
		this.imageAnchorX_ = 0;
		this.imageAnchorY_ = 0;
		this.imageHeight_ = 0;
		this.imageOpacity_ = 0;
		this.imageOriginX_ = 0;
		this.imageOriginY_ = 0;
		this.imageRotateWithView_ = false;
		this.imageRotation_ = 0;
		this.imageScale_ = [0, 0];
		this.imageWidth_ = 0;
		this.text_ = "";
		this.textOffsetX_ = 0;
		this.textOffsetY_ = 0;
		this.textRotateWithView_ = false;
		this.textRotation_ = 0;
		this.textScale_ = [0, 0];
		this.textFillState_ = null;
		this.textStrokeState_ = null;
		this.textState_ = null;
		this.pixelCoordinates_ = [];
		this.tmpLocalTransform_ = Se();
	}
	drawImages_(e, t, i, s) {
		if (!this.image_) return;
		const r = At(e, t, i, s, this.transform_, this.pixelCoordinates_), o = this.context_, a = this.tmpLocalTransform_, l = this.context_.globalAlpha;
		if (this.imageOpacity_ != 1) {
			this.context_.globalAlpha = o.globalAlpha * this.imageOpacity_;
		}
		let h = this.imageRotation_;
		if (this.transformRotation_ === 0) {
			h -= this.viewRotation_;
		}
		if (this.imageRotateWithView_) {
			h += this.viewRotation_;
		}
		for (let c = 0, u = r.length; c < u; c += 2) {
			const d = r[c] - this.imageAnchorX_;
			const f = r[c + 1] - this.imageAnchorY_;
			if (h !== 0 || this.imageScale_[0] != 1 || this.imageScale_[1] != 1) {
				const g = d + this.imageAnchorX_;
				const m = f + this.imageAnchorY_;
				gt(a, g, m, 1, 1, h, -g, -m);
				o.save();
				o.transform.apply(o, a);
				o.translate(g, m);
				o.scale(this.imageScale_[0], this.imageScale_[1]);
				o.drawImage(this.image_, this.imageOriginX_, this.imageOriginY_, this.imageWidth_, this.imageHeight_, -this.imageAnchorX_, -this.imageAnchorY_, this.imageWidth_, this.imageHeight_);
				o.restore();
			} else o.drawImage(this.image_, this.imageOriginX_, this.imageOriginY_, this.imageWidth_, this.imageHeight_, d, f, this.imageWidth_, this.imageHeight_);
		}
		if (this.imageOpacity_ != 1) {
			this.context_.globalAlpha = o.globalAlpha;
		}
	}
	drawText_(e, t, i, s) {
		if (!this.textState_ || this.text_ === "") return;
		if (this.textFillState_) {
			this.setContextFillState_(this.textFillState_);
		}
		if (this.textStrokeState_) {
			this.setContextStrokeState_(this.textStrokeState_);
		}
		this.setContextTextState_(this.textState_);
		const r = At(e, t, i, s, this.transform_, this.pixelCoordinates_), o = this.context_;
		let a = this.textRotation_;
		for (this.transformRotation_ === 0 && (a -= this.viewRotation_), this.textRotateWithView_ && (a += this.viewRotation_); t < i; t += s) {
			const l = r[t] + this.textOffsetX_;
			const h = r[t + 1] + this.textOffsetY_;
			a !== 0 || this.textScale_[0] != 1 || this.textScale_[1] != 1 ? (o.save(), o.translate(l - this.textOffsetX_, h - this.textOffsetY_), o.rotate(a), o.translate(this.textOffsetX_, this.textOffsetY_), o.scale(this.textScale_[0], this.textScale_[1]), this.textStrokeState_ && o.strokeText(this.text_, 0, 0), this.textFillState_ && o.fillText(this.text_, 0, 0), o.restore()) : (this.textStrokeState_ && o.strokeText(this.text_, l, h), this.textFillState_ && o.fillText(this.text_, l, h));
		}
	}
	moveToLineTo_(e, t, i, s, r, o) {
		const a = this.context_;
		let l = At(e, t, i, s, this.transform_, this.pixelCoordinates_);
		if (Math.abs(o) > 0) {
			l = F_(l, s, o, r, l);
		}
		this.context_.moveTo(l[0], l[1]);
		let h = l.length;
		if (r) {
			h -= 2;
		}
		for (let c = 2; c < h; c += 2) this.context_.lineTo(l[c], l[c + 1]);
		if (r) {
			this.context_.closePath();
		}
		return i;
	}
	drawRings_(e, t, i, s, r) {
		for (let o = 0, a = i.length; o < a; ++o) t = this.moveToLineTo_(e, t, i[o], s, true, r);
		return t;
	}
	drawCircle(e) {
		if (this.squaredTolerance_) {
			e = e.simplifyTransformed(this.squaredTolerance_, this.userTransform_);
		}
		if (!!me(this.extent_, e.getExtent())) {
			if (this.fillState_ || this.strokeState_) {
				if (this.fillState_) {
					this.setContextFillState_(this.fillState_);
				}
				if (this.strokeState_) {
					this.setContextStrokeState_(this.strokeState_);
				}
				const t = Wg(e, this.transform_, this.pixelCoordinates_);
				const i = t[2] - t[0];
				const s = t[3] - t[1];
				const r = Math.sqrt(i * i + s * s);
				const o = this.context_;
				this.context_.beginPath();
				this.context_.arc(t[0], t[1], r, 0, 2 * Math.PI);
				if (this.fillState_) {
					this.context_.fill();
				}
				if (this.strokeState_) {
					this.context_.stroke();
				}
			}
			if (this.text_ !== "") {
				this.drawText_(e.getCenter(), 0, 2, 2);
			}
		}
	}
	setStyle(e) {
		this.setFillStrokeStyle(e.getFill(), e.getStroke());
		this.setImageStyle(e.getImage());
		this.setTextStyle(e.getText());
	}
	setTransform(e) {
		this.transform_ = e;
	}
	drawGeometry(e) {
		switch (e.getType()) {
			case "Point":
				this.drawPoint(e);
				break;
			case "LineString":
				this.drawLineString(e);
				break;
			case "Polygon":
				this.drawPolygon(e);
				break;
			case "MultiPoint":
				this.drawMultiPoint(e);
				break;
			case "MultiLineString":
				this.drawMultiLineString(e);
				break;
			case "MultiPolygon":
				this.drawMultiPolygon(e);
				break;
			case "GeometryCollection":
				this.drawGeometryCollection(e);
				break;
			case "Circle":
				this.drawCircle(e);
				break;
		}
	}
	drawFeature(e, t) {
		const i = t.getGeometryFunction()(e);
		if (i) {
			this.setStyle(t);
			this.drawGeometry(i);
		}
	}
	drawGeometryCollection(e) {
		const t = e.getGeometriesArray();
		for (let i = 0, s = t.length; i < s; ++i) this.drawGeometry(t[i]);
	}
	drawPoint(e) {
		if (this.squaredTolerance_) {
			e = e.simplifyTransformed(this.squaredTolerance_, this.userTransform_);
		}
		const t = e.getFlatCoordinates(), i = e.getStride();
		if (this.image_) {
			this.drawImages_(t, 0, t.length, i);
		}
		if (this.text_ !== "") {
			this.drawText_(t, 0, t.length, i);
		}
	}
	drawMultiPoint(e) {
		if (this.squaredTolerance_) {
			e = e.simplifyTransformed(this.squaredTolerance_, this.userTransform_);
		}
		const t = e.getFlatCoordinates(), i = e.getStride();
		if (this.image_) {
			this.drawImages_(t, 0, t.length, i);
		}
		if (this.text_ !== "") {
			this.drawText_(t, 0, t.length, i);
		}
	}
	drawLineString(e) {
		if (this.squaredTolerance_) {
			e = e.simplifyTransformed(this.squaredTolerance_, this.userTransform_);
		}
		if (!!me(this.extent_, e.getExtent())) {
			if (this.strokeState_) {
				this.setContextStrokeState_(this.strokeState_);
				const t = this.context_;
				const i = e.getFlatCoordinates();
				this.context_.beginPath();
				this.moveToLineTo_(i, 0, i.length, e.getStride(), false, this.strokeState_.strokeOffset);
				this.context_.stroke();
			}
			if (this.text_ !== "") {
				const t = e.getFlatMidpoint();
				this.drawText_(t, 0, 2, 2);
			}
		}
	}
	drawMultiLineString(e) {
		if (this.squaredTolerance_) {
			e = e.simplifyTransformed(this.squaredTolerance_, this.userTransform_);
		}
		const t = e.getExtent();
		if (me(this.extent_, t)) {
			if (this.strokeState_) {
				this.setContextStrokeState_(this.strokeState_);
				const i = this.context_;
				const s = e.getFlatCoordinates();
				let r = 0;
				const o = e.getEnds();
				const a = e.getStride();
				this.context_.beginPath();
				for (let l = 0, h = o.length; l < h; ++l) r = this.moveToLineTo_(s, r, o[l], a, false, this.strokeState_.strokeOffset);
				this.context_.stroke();
			}
			if (this.text_ !== "") {
				const i = e.getFlatMidpoints();
				this.drawText_(i, 0, i.length, 2);
			}
		}
	}
	drawPolygon(e) {
		var t;
		if (this.squaredTolerance_) {
			e = e.simplifyTransformed(this.squaredTolerance_, this.userTransform_);
		}
		if (!!me(this.extent_, e.getExtent())) {
			if (this.strokeState_ || this.fillState_) {
				if (this.fillState_) {
					this.setContextFillState_(this.fillState_);
				}
				if (this.strokeState_) {
					this.setContextStrokeState_(this.strokeState_);
				}
				const i = this.context_;
				this.context_.beginPath();
				this.drawRings_(e.getOrientedFlatCoordinates(), 0, e.getEnds(), e.getStride(), (t = this.strokeState_) == null ? "undefined" : t.strokeOffset);
				if (this.fillState_) {
					this.context_.fill();
				}
				if (this.strokeState_) {
					this.context_.stroke();
				}
			}
			if (this.text_ !== "") {
				const i = e.getFlatInteriorPoint();
				this.drawText_(i, 0, 2, 2);
			}
		}
	}
	drawMultiPolygon(e) {
		var t;
		if (this.squaredTolerance_) {
			e = e.simplifyTransformed(this.squaredTolerance_, this.userTransform_);
		}
		if (!!me(this.extent_, e.getExtent())) {
			if (this.strokeState_ || this.fillState_) {
				if (this.fillState_) {
					this.setContextFillState_(this.fillState_);
				}
				if (this.strokeState_) {
					this.setContextStrokeState_(this.strokeState_);
				}
				const i = this.context_;
				const s = e.getOrientedFlatCoordinates();
				let r = 0;
				const o = e.getEndss();
				const a = e.getStride();
				this.context_.beginPath();
				for (let l = 0, h = o.length; l < h; ++l) {
					const c = o[l];
					r = this.drawRings_(s, r, o[l], a, (t = this.strokeState_) == null ? "undefined" : t.strokeOffset);
				}
				if (this.fillState_) {
					this.context_.fill();
				}
				if (this.strokeState_) {
					this.context_.stroke();
				}
			}
			if (this.text_ !== "") {
				const i = e.getFlatInteriorPoints();
				this.drawText_(i, 0, i.length, 2);
			}
		}
	}
	setContextFillState_(e) {
		const t = this.context_, i = this.contextFillState_;
		this.contextFillState_ ? this.contextFillState_.fillStyle != e.fillStyle && (this.contextFillState_.fillStyle = e.fillStyle, this.context_.fillStyle = e.fillStyle) : (this.context_.fillStyle = e.fillStyle, this.contextFillState_ = { fillStyle: e.fillStyle });
	}
	setContextStrokeState_(e) {
		const t = this.context_, i = this.contextStrokeState_;
		this.contextStrokeState_ ? (this.contextStrokeState_.lineCap != e.lineCap && (this.contextStrokeState_.lineCap = e.lineCap, this.context_.lineCap = e.lineCap), kt(this.contextStrokeState_.lineDash, e.lineDash) || this.context_.setLineDash(this.contextStrokeState_.lineDash = e.lineDash), this.contextStrokeState_.lineDashOffset != e.lineDashOffset && (this.contextStrokeState_.lineDashOffset = e.lineDashOffset, this.context_.lineDashOffset = e.lineDashOffset), this.contextStrokeState_.lineJoin != e.lineJoin && (this.contextStrokeState_.lineJoin = e.lineJoin, this.context_.lineJoin = e.lineJoin), this.contextStrokeState_.lineWidth != e.lineWidth && (this.contextStrokeState_.lineWidth = e.lineWidth, this.context_.lineWidth = e.lineWidth), this.contextStrokeState_.miterLimit != e.miterLimit && (this.contextStrokeState_.miterLimit = e.miterLimit, this.context_.miterLimit = e.miterLimit), this.contextStrokeState_.strokeStyle != e.strokeStyle && (this.contextStrokeState_.strokeStyle = e.strokeStyle, this.context_.strokeStyle = e.strokeStyle)) : (this.context_.lineCap = e.lineCap, this.context_.setLineDash(e.lineDash), this.context_.lineDashOffset = e.lineDashOffset, this.context_.lineJoin = e.lineJoin, this.context_.lineWidth = e.lineWidth, this.context_.miterLimit = e.miterLimit, this.context_.strokeStyle = e.strokeStyle, this.contextStrokeState_ = {
			lineCap: e.lineCap,
			lineDash: e.lineDash,
			lineDashOffset: e.lineDashOffset,
			lineJoin: e.lineJoin,
			lineWidth: e.lineWidth,
			miterLimit: e.miterLimit,
			strokeStyle: e.strokeStyle
		});
	}
	setContextTextState_(e) {
		const t = this.context_, i = this.contextTextState_, s = e.textAlign ? e.textAlign : Cs;
		this.contextTextState_ ? (this.contextTextState_.font != e.font && (this.contextTextState_.font = e.font, this.context_.font = e.font), this.contextTextState_.textAlign != s && (this.contextTextState_.textAlign = s, this.context_.textAlign = s), this.contextTextState_.textBaseline != e.textBaseline && (this.contextTextState_.textBaseline = e.textBaseline, this.context_.textBaseline = e.textBaseline)) : (this.context_.font = e.font, this.context_.textAlign = s, this.context_.textBaseline = e.textBaseline, this.contextTextState_ = {
			font: e.font,
			textAlign: s,
			textBaseline: e.textBaseline
		});
	}
	setFillStrokeStyle(e, t) {
		if (!e) this.fillState_ = null;
		else {
			const i = e.getColor();
			this.fillState_ = { fillStyle: It(i || Ve) };
		}
		if (!t) this.strokeState_ = null;
		else {
			const i = t.getColor();
			const s = t.getLineCap();
			const r = t.getLineDash();
			const o = t.getLineDashOffset();
			const a = t.getLineJoin();
			const l = t.getWidth();
			const h = t.getMiterLimit();
			const c = r || ti;
			const u = t.getOffset();
			this.strokeState_ = {
				lineCap: s !== "undefined" ? s : In,
				lineDash: this.pixelRatio_ === 1 ? c : c.map((d) => d * this.pixelRatio_),
				lineDashOffset: (o || ii) * this.pixelRatio_,
				lineJoin: a !== "undefined" ? a : Fn,
				lineWidth: (l !== "undefined" ? l : Rs) * this.pixelRatio_,
				miterLimit: h !== "undefined" ? h : Es,
				strokeStyle: It(i || Ts),
				strokeOffset: (u != null ? u : 0) * this.pixelRatio_
			};
		}
	}
	setImageStyle(e) {
		let t;
		if (!e || !(t = e.getSize())) {
			this.image_ = null;
			return;
		}
		const i = e.getPixelRatio(this.pixelRatio_), s = e.getAnchor(), r = e.getOrigin();
		this.image_ = e.getImage(this.pixelRatio_);
		this.imageAnchorX_ = s[0] * i;
		this.imageAnchorY_ = s[1] * i;
		this.imageHeight_ = t[1] * i;
		this.imageOpacity_ = e.getOpacity();
		this.imageOriginX_ = r[0];
		this.imageOriginY_ = r[1];
		this.imageRotateWithView_ = e.getRotateWithView();
		this.imageRotation_ = e.getRotation();
		const o = e.getScaleArray();
		this.imageScale_ = [o[0] * this.pixelRatio_ / i, o[1] * this.pixelRatio_ / i];
		this.imageWidth_ = t[0] * i;
	}
	setTextStyle(e) {
		if (!e) this.text_ = "";
		else {
			const t = e.getFill();
			if (!t) this.textFillState_ = null;
			else {
				const f = t.getColor();
				this.textFillState_ = { fillStyle: It(f || Ve) };
			}
			const i = e.getStroke();
			if (!i) this.textStrokeState_ = null;
			else {
				const f = i.getColor();
				const g = i.getLineCap();
				const m = i.getLineDash();
				const _ = i.getLineDashOffset();
				const p = i.getLineJoin();
				const y = i.getWidth();
				const E = i.getMiterLimit();
				this.textStrokeState_ = {
					lineCap: g !== "undefined" ? g : In,
					lineDash: m || ti,
					lineDashOffset: _ || ii,
					lineJoin: p !== "undefined" ? p : Fn,
					lineWidth: y !== "undefined" ? y : Rs,
					miterLimit: E !== "undefined" ? E : Es,
					strokeStyle: It(f || Ts)
				};
			}
			const s = e.getFont();
			const r = e.getOffsetX();
			const o = e.getOffsetY();
			const a = e.getRotateWithView();
			const l = e.getRotation();
			const h = e.getScaleArray();
			const c = e.getText();
			const u = e.getTextAlign();
			const d = e.getTextBaseline();
			this.textState_ = {
				font: s !== "undefined" ? s : Bu,
				textAlign: u !== "undefined" ? u : Cs,
				textBaseline: d !== "undefined" ? d : Ur
			};
			this.text_ = c !== "undefined" ? Array.isArray(c) ? c.reduce((f, g, m) => f += m % 2 ? " " : g, "") : c : "";
			this.textOffsetX_ = r !== "undefined" ? this.pixelRatio_ * r : 0;
			this.textOffsetY_ = o !== "undefined" ? this.pixelRatio_ * o : 0;
			this.textRotateWithView_ = a !== "undefined" ? a : false;
			this.textRotation_ = l !== "undefined" ? l : 0;
			this.textScale_ = [this.pixelRatio_ * h[0], this.pixelRatio_ * h[1]];
		}
	}
}
const j_ = .5;
const zu = {
	Point: H_,
	LineString: Y_,
	Polygon: J_,
	MultiPoint: q_,
	MultiLineString: Z_,
	MultiPolygon: K_,
	GeometryCollection: V_,
	Circle: X_
};
function z_(n, e) {
	return parseInt(O(n), 10) - parseInt(O(e), 10);
}
function Aa(n, e) {
	const t = Xu(n, e);
	return t * t;
}
function Xu(n, e) {
	return j_ * n / e;
}
function X_(n, e, t, i, s) {
	const r = t.getFill(), o = t.getStroke();
	if (r || o) {
		const l = n.getBuilder(t.getZIndex(), "Circle");
		l.setFillStrokeStyle(r, o);
		l.drawCircle(e, i, s);
	}
	const a = t.getText();
	if (a && a.getText()) {
		const l = n.getBuilder(t.getZIndex(), "Text");
		l.setTextStyle(a);
		l.drawText(e, i);
	}
}
function jr(n, e, t, i, s, r, o, a) {
	const l = [], h = t.getImage();
	if (h) {
		let d = true;
		const f = h.getImageState();
		f == $.LOADED || f == $.ERROR ? d = false : f == $.IDLE && h.load();
		l.push(h.ready());
	}
	const c = t.getFill();
	if (c && c.loading()) {
		l.push(c.ready());
	}
	const u = l.length > 0;
	if (u) {
		Promise.all(l).then(() => s(null));
	}
	W_(n, e, t, i, r, o, a);
	return u;
}
function W_(n, e, t, i, s, r, o) {
	const a = t.getGeometryFunction()(e);
	if (!a) return;
	const l = a.simplifyTransformed(i, s);
	if (t.getRenderer()) Wu(n, l, t, e, o);
	else {
		const c = zu[l.getType()];
		zu[l.getType()](n, l, t, e, o, r);
	}
}
function Wu(n, e, t, i, s) {
	if (e.getType() == "GeometryCollection") {
		const o = e.getGeometries();
		for (let a = 0, l = o.length; a < l; ++a) Wu(n, o[a], t, i, s);
		return;
	}
	n.getBuilder(t.getZIndex(), "Default").drawCustom(e, i, t.getRenderer(), t.getHitDetectionRenderer(), s);
}
function V_(n, e, t, i, s, r) {
	const o = e.getGeometriesArray();
	let a, l;
	for (a = 0, l = o.length; a < l; ++a) {
		const h = zu[o[a].getType()];
		zu[o[a].getType()](n, o[a], t, i, s, r);
	}
}
function Y_(n, e, t, i, s) {
	const r = t.getStroke();
	if (r) {
		const a = n.getBuilder(t.getZIndex(), "LineString");
		a.setFillStrokeStyle(null, r);
		a.drawLineString(e, i, s);
	}
	const o = t.getText();
	if (o && o.getText()) {
		const a = n.getBuilder(t.getZIndex(), "Text");
		a.setTextStyle(o);
		a.drawText(e, i, s);
	}
}
function Z_(n, e, t, i, s) {
	const r = t.getStroke();
	if (r) {
		const a = n.getBuilder(t.getZIndex(), "LineString");
		a.setFillStrokeStyle(null, r);
		a.drawMultiLineString(e, i, s);
	}
	const o = t.getText();
	if (o && o.getText()) {
		const a = n.getBuilder(t.getZIndex(), "Text");
		a.setTextStyle(o);
		a.drawText(e, i, s);
	}
}
function K_(n, e, t, i, s) {
	const r = t.getFill(), o = t.getStroke();
	if (o || r) {
		const l = n.getBuilder(t.getZIndex(), "Polygon");
		l.setFillStrokeStyle(r, o);
		l.drawMultiPolygon(e, i, s);
	}
	const a = t.getText();
	if (a && a.getText()) {
		const l = n.getBuilder(t.getZIndex(), "Text");
		l.setTextStyle(a);
		l.drawText(e, i, s);
	}
}
function H_(n, e, t, i, s, r) {
	const o = t.getImage(), a = t.getText(), l = a && a.getText(), h = r && o && l ? {} : "undefined";
	if (o) {
		if (o.getImageState() != $.LOADED) return;
		const c = n.getBuilder(t.getZIndex(), "Image");
		c.setImageStyle(o, h);
		c.drawPoint(e, i, s);
	}
	if (l) {
		const c = n.getBuilder(t.getZIndex(), "Text");
		c.setTextStyle(a, h);
		c.drawText(e, i, s);
	}
}
function q_(n, e, t, i, s, r) {
	const o = t.getImage(), a = o && o.getOpacity() !== 0, l = t.getText(), h = l && l.getText(), c = r && a && h ? {} : "undefined";
	if (a) {
		if (o.getImageState() != $.LOADED) return;
		const u = n.getBuilder(t.getZIndex(), "Image");
		u.setImageStyle(o, c);
		u.drawMultiPoint(e, i, s);
	}
	if (h) {
		const u = n.getBuilder(t.getZIndex(), "Text");
		u.setTextStyle(l, c);
		u.drawText(e, i, s);
	}
}
function J_(n, e, t, i, s) {
	const r = t.getFill(), o = t.getStroke();
	if (r || o) {
		const l = n.getBuilder(t.getZIndex(), "Polygon");
		l.setFillStrokeStyle(r, o);
		l.drawPolygon(e, i, s);
	}
	const a = t.getText();
	if (a && a.getText()) {
		const l = n.getBuilder(t.getZIndex(), "Text");
		l.setTextStyle(a);
		l.drawText(e, i, s);
	}
}
function BC(n, e) {
	const t = n.canvas;
	e = e || {};
	const i = e.pixelRatio || Lu, s = e.size;
	if (e.size) {
		n.canvas.width = e.size[0] * i;
		n.canvas.height = e.size[1] * i;
		n.canvas.style.width = e.size[0] + "px";
		n.canvas.style.height = e.size[1] + "px";
	}
	const r = [
		0,
		0,
		n.canvas.width,
		n.canvas.height
	], o = Er(Se(), i, i);
	return new U_(n, i, r, o, 0);
}
class Pl {
	constructor(e) {
		e = e || {};
		this.patternImage_ = null;
		this.color_ = null;
		if (e.color !== "undefined") {
			this.setColor(e.color);
		}
	}
	clone() {
		const e = this.getColor();
		return new Pl({ color: Array.isArray(e) ? e.slice() : e || "undefined" });
	}
	getColor() {
		return this.color_;
	}
	setColor(e) {
		if (e !== null && typeof e == "object" && "src" in e) {
			const t = hs(null, e.src, { crossOrigin: "anonymous" }, "undefined", e.offset ? null : e.color ? e.color : null, !(e.offset && e.size));
			t.ready().then(() => {
				this.patternImage_ = null;
			});
			if (t.getImageState() === $.IDLE) {
				t.load();
			}
			if (t.getImageState() === $.LOADING) {
				this.patternImage_ = t;
			}
		}
		this.color_ = e;
	}
	getKey() {
		const e = this.getColor();
		return e ? e instanceof CanvasPattern || e instanceof CanvasGradient ? O(e) : typeof e == "object" && "src" in e ? e.src + ":" + e.offset : _t(e).toString() : "";
	}
	loading() {
		return !!this.patternImage_;
	}
	ready() {
		return this.patternImage_ ? this.patternImage_.ready() : Promise.resolve();
	}
}
class Il {
	constructor(e) {
		e = e || {};
		this.color_ = e.color !== "undefined" ? e.color : null;
		this.lineCap_ = e.lineCap;
		this.lineDash_ = e.lineDash !== "undefined" ? e.lineDash : null;
		this.lineDashOffset_ = e.lineDashOffset;
		this.lineJoin_ = e.lineJoin;
		this.miterLimit_ = e.miterLimit;
		this.offset_ = e.offset;
		this.width_ = e.width;
	}
	clone() {
		const e = this.getColor();
		return new Il({
			color: Array.isArray(e) ? e.slice() : e || "undefined",
			lineCap: this.getLineCap(),
			lineDash: this.getLineDash() ? this.getLineDash().slice() : "undefined",
			lineDashOffset: this.getLineDashOffset(),
			lineJoin: this.getLineJoin(),
			miterLimit: this.getMiterLimit(),
			offset: this.getOffset(),
			width: this.getWidth()
		});
	}
	getColor() {
		return this.color_;
	}
	getLineCap() {
		return this.lineCap_;
	}
	getLineDash() {
		return this.lineDash_;
	}
	getLineDashOffset() {
		return this.lineDashOffset_;
	}
	getLineJoin() {
		return this.lineJoin_;
	}
	getMiterLimit() {
		return this.miterLimit_;
	}
	getOffset() {
		return this.offset_;
	}
	getWidth() {
		return this.width_;
	}
	setColor(e) {
		this.color_ = e;
	}
	setLineCap(e) {
		this.lineCap_ = e;
	}
	setLineDash(e) {
		this.lineDash_ = e;
	}
	setLineDashOffset(e) {
		this.lineDashOffset_ = e;
	}
	setLineJoin(e) {
		this.lineJoin_ = e;
	}
	setMiterLimit(e) {
		this.miterLimit_ = e;
	}
	setOffset(e) {
		this.offset_ = e;
	}
	setWidth(e) {
		this.width_ = e;
	}
}
function sc(n) {
	return n[0] > 0 && n[1] > 0;
}
function Q_(n, e, t) {
	t[0] = n[0] * e + .5 | 0;
	t[1] = n[1] * e + .5 | 0;
	return t;
}
function Ne(n, e) {
	return Array.isArray(n) ? n : (e === "undefined" ? e = [n, n] : (e[0] = n, e[1] = n), e);
}
class Fl {
	constructor(e) {
		this.opacity_ = e.opacity;
		this.rotateWithView_ = e.rotateWithView;
		this.rotation_ = e.rotation;
		this.scale_ = e.scale;
		this.scaleArray_ = Ne(e.scale);
		this.displacement_ = e.displacement;
		this.declutterMode_ = e.declutterMode;
	}
	clone() {
		const e = this.getScale();
		return new Fl({
			opacity: this.getOpacity(),
			scale: Array.isArray(e) ? e.slice() : e,
			rotation: this.getRotation(),
			rotateWithView: this.getRotateWithView(),
			displacement: this.getDisplacement().slice(),
			declutterMode: this.getDeclutterMode()
		});
	}
	getOpacity() {
		return this.opacity_;
	}
	getRotateWithView() {
		return this.rotateWithView_;
	}
	getRotation() {
		return this.rotation_;
	}
	getScale() {
		return this.scale_;
	}
	getScaleArray() {
		return this.scaleArray_;
	}
	getDisplacement() {
		return this.displacement_;
	}
	getDeclutterMode() {
		return this.declutterMode_;
	}
	getAnchor() {
		return z();
	}
	getImage(e) {
		return z();
	}
	getHitDetectionImage() {
		return z();
	}
	getPixelRatio(e) {
		return 1;
	}
	getImageState() {
		return z();
	}
	getImageSize() {
		return z();
	}
	getOrigin() {
		return z();
	}
	getSize() {
		return z();
	}
	setDisplacement(e) {
		this.displacement_ = e;
	}
	setOpacity(e) {
		this.opacity_ = e;
	}
	setRotateWithView(e) {
		this.rotateWithView_ = e;
	}
	setRotation(e) {
		this.rotation_ = e;
	}
	setScale(e) {
		this.scale_ = e;
		this.scaleArray_ = Ne(e);
	}
	listenImageChange(e) {
		z();
	}
	load() {
		z();
	}
	unlistenImageChange(e) {
		z();
	}
	ready() {
		return Promise.resolve();
	}
}
class Ll extends Fl {
	constructor(e) {
		super({
			opacity: 1,
			rotateWithView: e.rotateWithView !== "undefined" ? e.rotateWithView : false,
			rotation: e.rotation !== "undefined" ? e.rotation : 0,
			scale: e.scale !== "undefined" ? e.scale : 1,
			displacement: e.displacement !== "undefined" ? e.displacement : [0, 0],
			declutterMode: e.declutterMode
		});
		this.hitDetectionCanvas_ = null;
		this.fill_ = e.fill !== "undefined" ? e.fill : null;
		this.origin_ = [0, 0];
		this.points_ = e.points;
		this.radius = e.radius;
		this.radius2_ = e.radius2;
		this.angle_ = e.angle !== "undefined" ? e.angle : 0;
		this.stroke_ = e.stroke !== "undefined" ? e.stroke : null;
		this.size_;
		this.renderOptions_;
		this.imageState_ = this.fill_ && this.fill_.loading() ? $.LOADING : $.LOADED;
		if (this.imageState_ === $.LOADING) {
			this.ready().then(() => this.imageState_ = $.LOADED);
		}
		this.render();
	}
	clone() {
		const e = this.getScale(), t = new Ll({
			fill: this.getFill() ? this.getFill().clone() : "undefined",
			points: this.getPoints(),
			radius: this.getRadius(),
			radius2: this.getRadius2(),
			angle: this.getAngle(),
			stroke: this.getStroke() ? this.getStroke().clone() : "undefined",
			rotation: this.getRotation(),
			rotateWithView: this.getRotateWithView(),
			scale: Array.isArray(e) ? e.slice() : e,
			displacement: this.getDisplacement().slice(),
			declutterMode: this.getDeclutterMode()
		});
		t.setOpacity(this.getOpacity());
		return t;
	}
	getAnchor() {
		const e = this.size_, t = this.getDisplacement(), i = this.getScaleArray();
		return [this.size_[0] / 2 - t[0] / i[0], this.size_[1] / 2 + t[1] / i[1]];
	}
	getAngle() {
		return this.angle_;
	}
	getFill() {
		return this.fill_;
	}
	setFill(e) {
		this.fill_ = e;
		this.render();
	}
	getHitDetectionImage() {
		if (!this.hitDetectionCanvas_) {
			this.hitDetectionCanvas_ = this.createHitDetectionCanvas_(this.renderOptions_);
		}
		return this.hitDetectionCanvas_;
	}
	getImage(e) {
		var r, o;
		const t = (r = this.fill_) == null ? "undefined" : r.getKey(), i = "".concat(e, ",").concat(this.angle_, ",").concat(this.radius, ",").concat(this.radius2_, ",").concat(this.points_, ",").concat(t) + Object.values(this.renderOptions_).join(",");
		let s = (o = ct.get(i, null)) == null ? "undefined" : o.getImage(1);
		if (!s) {
			const a = this.renderOptions_;
			const l = Math.ceil(this.renderOptions_.size * e);
			const h = Re(l, l);
			this.draw_(this.renderOptions_, h, e);
			s = h.canvas;
			const c = new ku(s, "undefined", null, $.LOADED, null);
			ct.set(i, null, c);
			createImageBitmap(s).then((u) => {
				c.setImage(u);
			});
		}
		return s;
	}
	getPixelRatio(e) {
		return e;
	}
	getImageSize() {
		return this.size_;
	}
	getImageState() {
		return this.imageState_;
	}
	getOrigin() {
		return this.origin_;
	}
	getPoints() {
		return this.points_;
	}
	getRadius() {
		return this.radius;
	}
	setRadius(e) {
		if (this.radius !== e) {
			this.radius = e;
			this.render();
		}
	}
	getRadius2() {
		return this.radius2_;
	}
	setRadius2(e) {
		if (this.radius2_ !== e) {
			this.radius2_ = e;
			this.render();
		}
	}
	getSize() {
		return this.size_;
	}
	getStroke() {
		return this.stroke_;
	}
	setStroke(e) {
		this.stroke_ = e;
		this.render();
	}
	listenImageChange(e) {}
	load() {}
	unlistenImageChange(e) {}
	calculateLineJoinSize_(e, t, i) {
		if (t === 0 || this.points_ === null || e !== "bevel" && e !== "miter") return t;
		let s = this.radius, r = this.radius2_ === "undefined" ? s : this.radius2_;
		if (s < r) {
			const v = s;
			s = r;
			r = s;
		}
		const o = this.radius2_ === "undefined" ? this.points_ : this.points_ * 2, a = 2 * Math.PI / o, l = r * Math.sin(a), h = Math.sqrt(r * r - l * l), c = s - h, u = Math.sqrt(l * l + c * c), d = u / l;
		if (e === "miter" && d <= i) return d * t;
		const f = t / 2 / d, g = t / 2 * (c / u), _ = Math.sqrt((s + f) * (s + f) + g * g) - s;
		if (this.radius2_ === "undefined" || e === "bevel") return _ * 2;
		const p = s * Math.sin(a), y = Math.sqrt(s * s - p * p), E = r - y, T = Math.sqrt(p * p + E * E) / p;
		if (T <= i) {
			const v = T * t / 2 - r - s;
			return 2 * Math.max(_, v);
		}
		return _ * 2;
	}
	createRenderOptions() {
		var u, d, f, g, m, _;
		let e = In, t = Fn, i = 0, s = null, r = 0, o, a = 0;
		if (this.stroke_) {
			o = It((u = this.stroke_.getColor()) != null ? u : Ts);
			a = (d = this.stroke_.getWidth()) != null ? d : Rs;
			s = this.stroke_.getLineDash();
			r = (f = this.stroke_.getLineDashOffset()) != null ? f : 0;
			t = (g = this.stroke_.getLineJoin()) != null ? g : Fn;
			e = (m = this.stroke_.getLineCap()) != null ? m : In;
			i = (_ = this.stroke_.getMiterLimit()) != null ? _ : Es;
		}
		const l = this.calculateLineJoinSize_(t, a, i), h = Math.max(this.radius, this.radius2_ || 0), c = Math.ceil(2 * h + l);
		return {
			strokeStyle: o,
			strokeWidth: a,
			size: c,
			lineCap: e,
			lineDash: s,
			lineDashOffset: r,
			lineJoin: t,
			miterLimit: i
		};
	}
	render() {
		this.renderOptions_ = this.createRenderOptions();
		const e = this.renderOptions_.size;
		this.hitDetectionCanvas_ = null;
		this.size_ = [this.renderOptions_.size, this.renderOptions_.size];
	}
	draw_(e, t, i) {
		t.scale(i, i);
		t.translate(e.size / 2, e.size / 2);
		this.createPath_(t);
		if (this.fill_) {
			let s = this.fill_.getColor();
			if (s === null) {
				s = Ve;
			}
			t.fillStyle = It(s);
			t.fill();
		}
		if (e.strokeStyle) {
			t.strokeStyle = e.strokeStyle;
			t.lineWidth = e.strokeWidth;
			if (e.lineDash) {
				t.setLineDash(e.lineDash);
				t.lineDashOffset = e.lineDashOffset;
			}
			t.lineCap = e.lineCap;
			t.lineJoin = e.lineJoin;
			t.miterLimit = e.miterLimit;
			t.stroke();
		}
	}
	createHitDetectionCanvas_(e) {
		let t;
		if (this.fill_) {
			let i = this.fill_.getColor();
			let s = 0;
			if (typeof i == "string") {
				i = _t(i);
			}
			i === null ? s = 1 : Array.isArray(i) && (s = i.length === 4 ? i[3] : 1);
			t = Re(e.size, e.size);
			this.drawHitDetectionCanvas_(e, t);
		}
		return t ? t.canvas : this.getImage(1);
	}
	createPath_(e) {
		let t = this.points_;
		const i = this.radius;
		if (t === null) e.arc(0, 0, this.radius, 0, 2 * Math.PI);
		else {
			const s = this.radius2_ === "undefined" ? i : this.radius2_;
			if (this.radius2_ !== "undefined") {
				t *= 2;
			}
			const r = this.angle_ - Math.PI / 2;
			const o = 2 * Math.PI / t;
			for (let a = 0; a < t; a++) {
				const l = r + a * o;
				const h = a % 2 === 0 ? i : s;
				e.lineTo(h * Math.cos(l), h * Math.sin(l));
			}
			e.closePath();
		}
	}
	drawHitDetectionCanvas_(e, t) {
		t.translate(e.size / 2, e.size / 2);
		this.createPath_(t);
		t.fillStyle = Ve;
		t.fill();
		if (e.strokeStyle) {
			t.strokeStyle = e.strokeStyle;
			t.lineWidth = e.strokeWidth;
			if (e.lineDash) {
				t.setLineDash(e.lineDash);
				t.lineDashOffset = e.lineDashOffset;
			}
			t.lineJoin = e.lineJoin;
			t.miterLimit = e.miterLimit;
			t.stroke();
		}
	}
	ready() {
		return this.fill_ ? this.fill_.ready() : Promise.resolve();
	}
}
class Al extends Ll {
	constructor(e) {
		e = e || { radius: 5 };
		super({
			points: null,
			fill: e.fill,
			radius: e.radius,
			stroke: e.stroke,
			scale: e.scale !== "undefined" ? e.scale : 1,
			rotation: e.rotation !== "undefined" ? e.rotation : 0,
			rotateWithView: e.rotateWithView !== "undefined" ? e.rotateWithView : false,
			displacement: e.displacement !== "undefined" ? e.displacement : [0, 0],
			declutterMode: e.declutterMode
		});
	}
	clone() {
		const e = this.getScale(), t = new Al({
			fill: this.getFill() ? this.getFill().clone() : "undefined",
			stroke: this.getStroke() ? this.getStroke().clone() : "undefined",
			radius: this.getRadius(),
			scale: Array.isArray(e) ? e.slice() : e,
			rotation: this.getRotation(),
			rotateWithView: this.getRotateWithView(),
			displacement: this.getDisplacement().slice(),
			declutterMode: this.getDeclutterMode()
		});
		t.setOpacity(this.getOpacity());
		return t;
	}
}
class yi {
	constructor(e) {
		e = e || {};
		this.geometry_ = null;
		this.geometryFunction_ = rc;
		if (e.geometry !== "undefined") {
			this.setGeometry(e.geometry);
		}
		this.fill_ = e.fill !== "undefined" ? e.fill : null;
		this.image_ = e.image !== "undefined" ? e.image : null;
		this.renderer_ = e.renderer !== "undefined" ? e.renderer : null;
		this.hitDetectionRenderer_ = e.hitDetectionRenderer !== "undefined" ? e.hitDetectionRenderer : null;
		this.stroke_ = e.stroke !== "undefined" ? e.stroke : null;
		this.text_ = e.text !== "undefined" ? e.text : null;
		this.zIndex_ = e.zIndex;
	}
	clone() {
		var t;
		let e = this.getGeometry();
		if (e && typeof e == "object") {
			e = e.clone();
		}
		return new yi({
			geometry: e != null ? e : "undefined",
			fill: this.getFill() ? this.getFill().clone() : "undefined",
			image: this.getImage() ? this.getImage().clone() : "undefined",
			renderer: (t = this.getRenderer()) != null ? t : "undefined",
			stroke: this.getStroke() ? this.getStroke().clone() : "undefined",
			text: this.getText() ? this.getText().clone() : "undefined",
			zIndex: this.getZIndex()
		});
	}
	getRenderer() {
		return this.renderer_;
	}
	setRenderer(e) {
		this.renderer_ = e;
	}
	setHitDetectionRenderer(e) {
		this.hitDetectionRenderer_ = e;
	}
	getHitDetectionRenderer() {
		return this.hitDetectionRenderer_;
	}
	getGeometry() {
		return this.geometry_;
	}
	getGeometryFunction() {
		return this.geometryFunction_;
	}
	getFill() {
		return this.fill_;
	}
	setFill(e) {
		this.fill_ = e;
	}
	getImage() {
		return this.image_;
	}
	setImage(e) {
		this.image_ = e;
	}
	getStroke() {
		return this.stroke_;
	}
	setStroke(e) {
		this.stroke_ = e;
	}
	getText() {
		return this.text_;
	}
	setText(e) {
		this.text_ = e;
	}
	getZIndex() {
		return this.zIndex_;
	}
	setGeometry(e) {
		typeof e == "function" ? this.geometryFunction_ = e : typeof e == "string" ? this.geometryFunction_ = function(t) {
			return t.get(e);
		} : e ? e !== "undefined" && (this.geometryFunction_ = function() {
			return e;
		}) : this.geometryFunction_ = rc;
		this.geometry_ = e;
	}
	setZIndex(e) {
		this.zIndex_ = e;
	}
}
function em(n) {
	let e;
	if (typeof n == "function") e = n;
	else {
		let t;
		Array.isArray(n) ? t = n : (ee(typeof n.getZIndex == "function", "Expected an `Style` or an array of `Style`"), t = [n]);
		e = function() {
			return t;
		};
	}
	return e;
}
let Xo = null;
function Zu(n, e) {
	const t = new Pl({ color: "rgba(255,255,255,0.4)" });
	const i = new Il({
		color: "#3399CC",
		width: 1.25
	});
	Xo = [new yi({
		image: new Al({
			fill: t,
			stroke: i,
			radius: 5
		}),
		fill: t,
		stroke: i
	})];
	return Xo;
}
function bl() {
	const n = {}, e = [
		255,
		255,
		255,
		1
	], t = [
		0,
		153,
		255,
		1
	], i = 3;
	n.Polygon = [new yi({ fill: new Pl({ color: [
		255,
		255,
		255,
		.5
	] }) })];
	n.MultiPolygon = n.Polygon;
	n.LineString = [new yi({ stroke: new Il({
		color: e,
		width: 5
	}) }), new yi({ stroke: new Il({
		color: t,
		width: 3
	}) })];
	n.MultiLineString = n.LineString;
	n.Circle = n.Polygon.concat(n.LineString);
	n.Point = [new yi({
		image: new Al({
			radius: 6,
			fill: new Pl({ color: t }),
			stroke: new Il({
				color: e,
				width: 1.5
			})
		}),
		zIndex: null
	})];
	n.MultiPoint = n.Point;
	n.GeometryCollection = n.Polygon.concat(n.LineString, n.Point);
	return n;
}
function rc(n) {
	return n.getGeometry();
}
const tm = "#333";
class Ol {
	constructor(e) {
		e = e || {};
		this.font_ = e.font;
		this.rotation_ = e.rotation;
		this.rotateWithView_ = e.rotateWithView;
		this.keepUpright_ = e.keepUpright;
		this.scale_ = e.scale;
		this.scaleArray_ = Ne(e.scale !== "undefined" ? e.scale : 1);
		this.text_ = e.text;
		this.textAlign_ = e.textAlign;
		this.justify_ = e.justify;
		this.repeat_ = e.repeat;
		this.textBaseline_ = e.textBaseline;
		this.fill_ = e.fill !== "undefined" ? e.fill : new Pl({ color: tm });
		this.maxAngle_ = e.maxAngle !== "undefined" ? e.maxAngle : Math.PI / 4;
		this.placement_ = e.placement !== "undefined" ? e.placement : "point";
		this.overflow_ = !!e.overflow;
		this.stroke_ = e.stroke !== "undefined" ? e.stroke : null;
		this.offsetX_ = e.offsetX !== "undefined" ? e.offsetX : 0;
		this.offsetY_ = e.offsetY !== "undefined" ? e.offsetY : 0;
		this.backgroundFill_ = e.backgroundFill ? e.backgroundFill : null;
		this.backgroundStroke_ = e.backgroundStroke ? e.backgroundStroke : null;
		this.padding_ = e.padding === "undefined" ? null : e.padding;
		this.declutterMode_ = e.declutterMode;
	}
	clone() {
		const e = this.getScale();
		return new Ol({
			font: this.getFont(),
			placement: this.getPlacement(),
			repeat: this.getRepeat(),
			maxAngle: this.getMaxAngle(),
			overflow: this.getOverflow(),
			rotation: this.getRotation(),
			rotateWithView: this.getRotateWithView(),
			keepUpright: this.getKeepUpright(),
			scale: Array.isArray(e) ? e.slice() : e,
			text: this.getText(),
			textAlign: this.getTextAlign(),
			justify: this.getJustify(),
			textBaseline: this.getTextBaseline(),
			fill: this.getFill() instanceof Pl ? this.getFill().clone() : this.getFill(),
			stroke: this.getStroke() ? this.getStroke().clone() : "undefined",
			offsetX: this.getOffsetX(),
			offsetY: this.getOffsetY(),
			backgroundFill: this.getBackgroundFill() ? this.getBackgroundFill().clone() : "undefined",
			backgroundStroke: this.getBackgroundStroke() ? this.getBackgroundStroke().clone() : "undefined",
			padding: this.getPadding() || "undefined",
			declutterMode: this.getDeclutterMode()
		});
	}
	getOverflow() {
		return this.overflow_;
	}
	getFont() {
		return this.font_;
	}
	getMaxAngle() {
		return this.maxAngle_;
	}
	getPlacement() {
		return this.placement_;
	}
	getRepeat() {
		return this.repeat_;
	}
	getOffsetX() {
		return this.offsetX_;
	}
	getOffsetY() {
		return this.offsetY_;
	}
	getFill() {
		return this.fill_;
	}
	getRotateWithView() {
		return this.rotateWithView_;
	}
	getKeepUpright() {
		return this.keepUpright_;
	}
	getRotation() {
		return this.rotation_;
	}
	getScale() {
		return this.scale_;
	}
	getScaleArray() {
		return this.scaleArray_;
	}
	getStroke() {
		return this.stroke_;
	}
	getText() {
		return this.text_;
	}
	getTextAlign() {
		return this.textAlign_;
	}
	getJustify() {
		return this.justify_;
	}
	getTextBaseline() {
		return this.textBaseline_;
	}
	getBackgroundFill() {
		return this.backgroundFill_;
	}
	getBackgroundStroke() {
		return this.backgroundStroke_;
	}
	getPadding() {
		return this.padding_;
	}
	getDeclutterMode() {
		return this.declutterMode_;
	}
	setOverflow(e) {
		this.overflow_ = e;
	}
	setFont(e) {
		this.font_ = e;
	}
	setMaxAngle(e) {
		this.maxAngle_ = e;
	}
	setOffsetX(e) {
		this.offsetX_ = e;
	}
	setOffsetY(e) {
		this.offsetY_ = e;
	}
	setPlacement(e) {
		this.placement_ = e;
	}
	setRepeat(e) {
		this.repeat_ = e;
	}
	setRotateWithView(e) {
		this.rotateWithView_ = e;
	}
	setKeepUpright(e) {
		this.keepUpright_ = e;
	}
	setFill(e) {
		this.fill_ = e;
	}
	setRotation(e) {
		this.rotation_ = e;
	}
	setScale(e) {
		this.scale_ = e;
		this.scaleArray_ = Ne(e !== "undefined" ? e : 1);
	}
	setStroke(e) {
		this.stroke_ = e;
	}
	setText(e) {
		this.text_ = e;
	}
	setTextAlign(e) {
		this.textAlign_ = e;
	}
	setJustify(e) {
		this.justify_ = e;
	}
	setTextBaseline(e) {
		this.textBaseline_ = e;
	}
	setBackgroundFill(e) {
		this.backgroundFill_ = e;
	}
	setBackgroundStroke(e) {
		this.backgroundStroke_ = e;
	}
	setPadding(e) {
		this.padding_ = e;
	}
}
const de = {
	ANIMATING: 0,
	INTERACTING: 1
};
const ks = {
	BEGIN_GEOMETRY: 0,
	BEGIN_PATH: 1,
	CIRCLE: 2,
	CLOSE_PATH: 3,
	CUSTOM: 4,
	DRAW_CHARS: 5,
	DRAW_IMAGE: 6,
	END_GEOMETRY: 7,
	FILL: 8,
	MOVE_TO_LINE_TO: 9,
	SET_FILL_STYLE: 10,
	SET_STROKE_STYLE: 11,
	STROKE: 12
};
const ir = [ks.FILL];
const xi = [ks.STROKE];
const $i = [ks.BEGIN_PATH];
const oc = [ks.CLOSE_PATH];
class nm extends L_ {
	constructor(e, t, i, s) {
		super();
		this.tolerance = e;
		this.maxExtent = t;
		this.pixelRatio = s;
		this.maxLineWidth = 0;
		this.resolution = i;
		this.beginGeometryInstruction1_ = null;
		this.beginGeometryInstruction2_ = null;
		this.bufferedMaxExtent_ = null;
		this.instructions = [];
		this.coordinates = [];
		this.tmpCoordinate_ = [];
		this.hitDetectionInstructions = [];
		this.state = {};
	}
	applyPixelRatio(e) {
		const t = this.pixelRatio;
		return this.pixelRatio == 1 ? e : e.map(function(i) {
			return i * t;
		});
	}
	appendFlatPointCoordinates(e, t) {
		const i = this.getBufferedMaxExtent(), s = this.tmpCoordinate_, r = this.coordinates;
		let o = this.coordinates.length;
		for (let a = 0, l = e.length; a < l; a += t) {
			this.tmpCoordinate_[0] = e[a];
			this.tmpCoordinate_[1] = e[a + 1];
			if (Ui(i, this.tmpCoordinate_)) {
				this.coordinates[o++] = this.tmpCoordinate_[0];
				this.coordinates[o++] = this.tmpCoordinate_[1];
			}
		}
		return o;
	}
	appendFlatLineCoordinates(e, t, i, s, r, o) {
		const a = this.coordinates;
		let l = this.coordinates.length;
		const h = this.getBufferedMaxExtent();
		if (o) {
			t += s;
		}
		let c = e[t], u = e[t + 1];
		const d = this.tmpCoordinate_;
		let f = true, g, m, _;
		for (g = t + s; g < i; g += s) {
			this.tmpCoordinate_[0] = e[g];
			this.tmpCoordinate_[1] = e[g + 1];
			_ = pa(h, this.tmpCoordinate_);
			_ !== m ? (f && (this.coordinates[l++] = c, this.coordinates[l++] = u, f = false), this.coordinates[l++] = this.tmpCoordinate_[0], this.coordinates[l++] = this.tmpCoordinate_[1]) : _ === Le.INTERSECTING ? (this.coordinates[l++] = this.tmpCoordinate_[0], this.coordinates[l++] = this.tmpCoordinate_[1], f = false) : f = true;
			c = this.tmpCoordinate_[0];
			u = this.tmpCoordinate_[1];
			m = _;
		}
		if (r && f || g === t + s) {
			this.coordinates[l++] = c;
			this.coordinates[l++] = u;
		}
		return l;
	}
	drawCustomCoordinates_(e, t, i, s, r) {
		for (let o = 0, a = i.length; o < a; ++o) {
			const l = i[o];
			const h = this.appendFlatLineCoordinates(e, t, i[o], s, false, false);
			r.push(h);
			t = i[o];
		}
		return t;
	}
	drawCustom(e, t, i, s, r) {
		this.beginGeometry(e, t, r);
		const o = e.getType(), a = e.getStride(), l = this.coordinates.length;
		let h, c, u, d, f;
		switch (o) {
			case "MultiPolygon":
				h = e.getOrientedFlatCoordinates(), d = [];
				const g = e.getEndss();
				f = 0;
				for (let m = 0, _ = g.length; m < _; ++m) {
					const p = [];
					f = this.drawCustomCoordinates_(h, f, g[m], a, p);
					d.push(p);
				}
				this.instructions.push([
					ks.CUSTOM,
					this.coordinates.length,
					d,
					e,
					i,
					Pa,
					r
				]), this.hitDetectionInstructions.push([
					ks.CUSTOM,
					this.coordinates.length,
					d,
					e,
					s || i,
					Pa,
					r
				]);
				break;
			case "Polygon":
			case "MultiLineString":
				u = [], h = o == "Polygon" ? e.getOrientedFlatCoordinates() : e.getFlatCoordinates(), f = this.drawCustomCoordinates_(h, 0, e.getEnds(), a, u), this.instructions.push([
					ks.CUSTOM,
					this.coordinates.length,
					u,
					e,
					i,
					xs,
					r
				]), this.hitDetectionInstructions.push([
					ks.CUSTOM,
					this.coordinates.length,
					u,
					e,
					s || i,
					xs,
					r
				]);
				break;
			case "LineString":
			case "Circle":
				h = e.getFlatCoordinates(), c = this.appendFlatLineCoordinates(h, 0, h.length, a, false, false), this.instructions.push([
					ks.CUSTOM,
					this.coordinates.length,
					c,
					e,
					i,
					mi,
					r
				]), this.hitDetectionInstructions.push([
					ks.CUSTOM,
					this.coordinates.length,
					c,
					e,
					s || i,
					mi,
					r
				]);
				break;
			case "MultiPoint":
				h = e.getFlatCoordinates(), c = this.appendFlatPointCoordinates(h, a), c > this.coordinates.length && (this.instructions.push([
					ks.CUSTOM,
					this.coordinates.length,
					c,
					e,
					i,
					mi,
					r
				]), this.hitDetectionInstructions.push([
					ks.CUSTOM,
					this.coordinates.length,
					c,
					e,
					s || i,
					mi,
					r
				]));
				break;
			case "Point":
				h = e.getFlatCoordinates(), this.coordinates.push(h[0], h[1]), c = this.coordinates.length, this.instructions.push([
					ks.CUSTOM,
					this.coordinates.length,
					c,
					e,
					i,
					"undefined",
					r
				]), this.hitDetectionInstructions.push([
					ks.CUSTOM,
					this.coordinates.length,
					c,
					e,
					s || i,
					"undefined",
					r
				]);
				break;
		}
		this.endGeometry(t);
	}
	beginGeometry(e, t, i) {
		this.beginGeometryInstruction1_ = [
			ks.BEGIN_GEOMETRY,
			t,
			0,
			e,
			i
		];
		this.instructions.push(this.beginGeometryInstruction1_);
		this.beginGeometryInstruction2_ = [
			ks.BEGIN_GEOMETRY,
			t,
			0,
			e,
			i
		];
		this.hitDetectionInstructions.push(this.beginGeometryInstruction2_);
	}
	finish() {
		return {
			instructions: this.instructions,
			hitDetectionInstructions: this.hitDetectionInstructions,
			coordinates: this.coordinates
		};
	}
	reverseHitDetectionInstructions() {
		const e = this.hitDetectionInstructions;
		this.hitDetectionInstructions.reverse();
		let t;
		const i = this.hitDetectionInstructions.length;
		let s, r, o = -1;
		for (t = 0; t < this.hitDetectionInstructions.length; ++t) {
			s = this.hitDetectionInstructions[t];
			r = s[0];
			r == ks.END_GEOMETRY ? o = t : r == ks.BEGIN_GEOMETRY && (s[2] = t, Sf(this.hitDetectionInstructions, o, t), o = -1);
		}
	}
	fillStyleToState(e, t = {}) {
		if (e) {
			const i = e.getColor();
			t.fillPatternScale = i && typeof i == "object" && "src" in i ? this.pixelRatio : 1;
			t.fillStyle = It(i || Ve);
		} else t.fillStyle = "undefined";
		return t;
	}
	strokeStyleToState(e, t = {}) {
		if (e) {
			const i = e.getColor();
			t.strokeStyle = It(i || Ts);
			const s = e.getLineCap();
			t.lineCap = s !== "undefined" ? s : In;
			const r = e.getLineDash();
			t.lineDash = r ? r.slice() : ti;
			const o = e.getLineDashOffset();
			t.lineDashOffset = o || ii;
			const a = e.getLineJoin();
			t.lineJoin = a !== "undefined" ? a : Fn;
			const l = e.getWidth();
			t.lineWidth = l !== "undefined" ? l : Rs;
			const h = e.getMiterLimit();
			t.miterLimit = h !== "undefined" ? h : Es;
			const c = e.getOffset();
			t.strokeOffset = c != null ? c : b_;
			if (t.lineWidth > this.maxLineWidth) {
				this.maxLineWidth = t.lineWidth;
				this.bufferedMaxExtent_ = null;
			}
		} else {
			t.strokeStyle = "undefined";
			t.lineCap = "undefined";
			t.lineDash = null;
			t.lineDashOffset = "undefined";
			t.lineJoin = "undefined";
			t.lineWidth = "undefined";
			t.miterLimit = "undefined";
			t.strokeOffset = "undefined";
		}
		return t;
	}
	setFillStrokeStyle(e, t) {
		const i = this.state;
		this.fillStyleToState(e, this.state);
		this.strokeStyleToState(t, this.state);
	}
	createFill(e) {
		const t = e.fillStyle, i = [ks.SET_FILL_STYLE, e.fillStyle];
		if (typeof e.fillStyle != "string") {
			i.push(e.fillPatternScale);
		}
		return i;
	}
	applyStroke(e) {
		this.instructions.push(this.createStroke(e));
	}
	createStroke(e) {
		return [
			ks.SET_STROKE_STYLE,
			e.strokeStyle,
			e.lineWidth * this.pixelRatio,
			e.lineCap,
			e.lineJoin,
			e.miterLimit,
			e.lineDash ? this.applyPixelRatio(e.lineDash) : null,
			e.lineDashOffset * this.pixelRatio
		];
	}
	updateFillStyle(e, t) {
		const i = e.fillStyle;
		if (typeof e.fillStyle != "string" || e.currentFillStyle != e.fillStyle) {
			this.instructions.push(t(e));
			e.currentFillStyle = e.fillStyle;
		}
	}
	updateStrokeStyle(e, t) {
		const i = e.strokeStyle, s = e.lineCap, r = e.lineDash, o = e.lineDashOffset, a = e.lineJoin, l = e.lineWidth, h = e.miterLimit, c = e.strokeOffset;
		if (e.currentStrokeStyle != e.strokeStyle || e.currentLineCap != e.lineCap || e.lineDash != e.currentLineDash && !kt(e.currentLineDash, e.lineDash) || e.currentLineDashOffset != e.lineDashOffset || e.currentLineJoin != e.lineJoin || e.currentLineWidth != e.lineWidth || e.currentMiterLimit != e.miterLimit || e.currentStrokeOffset != e.strokeOffset) {
			t(e);
			e.currentStrokeStyle = e.strokeStyle;
			e.currentLineCap = e.lineCap;
			e.currentLineDash = e.lineDash;
			e.currentLineDashOffset = e.lineDashOffset;
			e.currentLineJoin = e.lineJoin;
			e.currentLineWidth = e.lineWidth;
			e.currentMiterLimit = e.miterLimit;
			e.currentStrokeOffset = e.strokeOffset;
		}
	}
	endGeometry(e) {
		this.beginGeometryInstruction1_[2] = this.instructions.length;
		this.beginGeometryInstruction1_ = null;
		this.beginGeometryInstruction2_[2] = this.hitDetectionInstructions.length;
		this.beginGeometryInstruction2_ = null;
		const t = [ks.END_GEOMETRY, e];
		this.instructions.push(t);
		this.hitDetectionInstructions.push(t);
	}
	getBufferedMaxExtent() {
		if (!this.bufferedMaxExtent_ && (this.bufferedMaxExtent_ = Kc(this.maxExtent), this.maxLineWidth > 0)) {
			const e = this.resolution * (this.maxLineWidth + 1) / 2;
			tt(this.bufferedMaxExtent_, e, this.bufferedMaxExtent_);
		}
		return this.bufferedMaxExtent_;
	}
}
class sm extends nm {
	constructor(e, t, i, s) {
		super(e, t, i, s);
		this.hitDetectionImage_ = null;
		this.image_ = null;
		this.imagePixelRatio_ = "undefined";
		this.anchorX_ = "undefined";
		this.anchorY_ = "undefined";
		this.height_ = "undefined";
		this.opacity_ = "undefined";
		this.originX_ = "undefined";
		this.originY_ = "undefined";
		this.rotateWithView_ = "undefined";
		this.rotation_ = "undefined";
		this.scale_ = "undefined";
		this.width_ = "undefined";
		this.declutterMode_ = "undefined";
		this.declutterImageWithText_ = "undefined";
	}
	drawPoint(e, t, i) {
		if (!this.image_ || this.maxExtent && !Ui(this.maxExtent, e.getFlatCoordinates())) return;
		this.beginGeometry(e, t, i);
		const s = e.getFlatCoordinates(), r = e.getStride(), o = this.coordinates.length, a = this.appendFlatPointCoordinates(s, r);
		this.instructions.push([
			ks.DRAW_IMAGE,
			this.coordinates.length,
			a,
			this.image_,
			this.anchorX_ * this.imagePixelRatio_,
			this.anchorY_ * this.imagePixelRatio_,
			Math.ceil(this.height_ * this.imagePixelRatio_),
			this.opacity_,
			this.originX_ * this.imagePixelRatio_,
			this.originY_ * this.imagePixelRatio_,
			this.rotateWithView_,
			this.rotation_,
			[this.scale_[0] * this.pixelRatio / this.imagePixelRatio_, this.scale_[1] * this.pixelRatio / this.imagePixelRatio_],
			Math.ceil(this.width_ * this.imagePixelRatio_),
			this.declutterMode_,
			this.declutterImageWithText_
		]);
		this.hitDetectionInstructions.push([
			ks.DRAW_IMAGE,
			this.coordinates.length,
			a,
			this.hitDetectionImage_,
			this.anchorX_,
			this.anchorY_,
			this.height_,
			1,
			this.originX_,
			this.originY_,
			this.rotateWithView_,
			this.rotation_,
			this.scale_,
			this.width_,
			this.declutterMode_,
			this.declutterImageWithText_
		]);
		this.endGeometry(t);
	}
	drawMultiPoint(e, t, i) {
		if (!this.image_) return;
		this.beginGeometry(e, t, i);
		const s = e.getFlatCoordinates(), r = [];
		for (let l = 0, h = s.length; l < h; l += e.getStride()) (!this.maxExtent || Ui(this.maxExtent, s.slice(l, l + 2))) && r.push(s[l], s[l + 1]);
		const o = this.coordinates.length, a = this.appendFlatPointCoordinates(r, 2);
		this.instructions.push([
			ks.DRAW_IMAGE,
			this.coordinates.length,
			a,
			this.image_,
			this.anchorX_ * this.imagePixelRatio_,
			this.anchorY_ * this.imagePixelRatio_,
			Math.ceil(this.height_ * this.imagePixelRatio_),
			this.opacity_,
			this.originX_ * this.imagePixelRatio_,
			this.originY_ * this.imagePixelRatio_,
			this.rotateWithView_,
			this.rotation_,
			[this.scale_[0] * this.pixelRatio / this.imagePixelRatio_, this.scale_[1] * this.pixelRatio / this.imagePixelRatio_],
			Math.ceil(this.width_ * this.imagePixelRatio_),
			this.declutterMode_,
			this.declutterImageWithText_
		]);
		this.hitDetectionInstructions.push([
			ks.DRAW_IMAGE,
			this.coordinates.length,
			a,
			this.hitDetectionImage_,
			this.anchorX_,
			this.anchorY_,
			this.height_,
			1,
			this.originX_,
			this.originY_,
			this.rotateWithView_,
			this.rotation_,
			this.scale_,
			this.width_,
			this.declutterMode_,
			this.declutterImageWithText_
		]);
		this.endGeometry(t);
	}
	finish() {
		this.reverseHitDetectionInstructions();
		this.anchorX_ = "undefined";
		this.anchorY_ = "undefined";
		this.hitDetectionImage_ = null;
		this.image_ = null;
		this.imagePixelRatio_ = "undefined";
		this.height_ = "undefined";
		this.scale_ = "undefined";
		this.opacity_ = "undefined";
		this.originX_ = "undefined";
		this.originY_ = "undefined";
		this.rotateWithView_ = "undefined";
		this.rotation_ = "undefined";
		this.width_ = "undefined";
		return super.finish();
	}
	setImageStyle(e, t) {
		const i = e.getAnchor(), s = e.getSize(), r = e.getOrigin();
		this.imagePixelRatio_ = e.getPixelRatio(this.pixelRatio);
		this.anchorX_ = i[0];
		this.anchorY_ = i[1];
		this.hitDetectionImage_ = e.getHitDetectionImage();
		this.image_ = e.getImage(this.pixelRatio);
		this.height_ = s[1];
		this.opacity_ = e.getOpacity();
		this.originX_ = r[0];
		this.originY_ = r[1];
		this.rotateWithView_ = e.getRotateWithView();
		this.rotation_ = e.getRotation();
		this.scale_ = e.getScaleArray();
		this.width_ = s[0];
		this.declutterMode_ = e.getDeclutterMode();
		this.declutterImageWithText_ = t;
	}
}
class om extends nm {
	constructor(e, t, i, s) {
		super(e, t, i, s);
	}
	drawFlatCoordinates_(e, t, i, s, r) {
		const o = this.coordinates.length, a = this.appendFlatLineCoordinates(e, t, i, s, false, false);
		this.instructions.push([
			ks.MOVE_TO_LINE_TO,
			this.coordinates.length,
			a,
			r * this.pixelRatio
		]);
		this.hitDetectionInstructions.push([
			ks.MOVE_TO_LINE_TO,
			this.coordinates.length,
			a,
			r
		]);
		return i;
	}
	drawLineString(e, t, i) {
		const s = this.state, r = this.state.strokeStyle, o = this.state.lineWidth, a = this.state.strokeOffset;
		if (this.state.strokeStyle === "undefined" || this.state.lineWidth === "undefined") return;
		this.updateStrokeStyle(this.state, this.applyStroke);
		this.beginGeometry(e, t, i);
		this.hitDetectionInstructions.push([
			ks.SET_STROKE_STYLE,
			this.state.strokeStyle,
			this.state.lineWidth,
			this.state.lineCap,
			this.state.lineJoin,
			this.state.miterLimit,
			ti,
			ii
		], $i);
		const l = e.getFlatCoordinates(), h = e.getStride();
		this.drawFlatCoordinates_(l, 0, l.length, h, this.state.strokeOffset);
		this.hitDetectionInstructions.push(xi);
		this.endGeometry(t);
	}
	drawMultiLineString(e, t, i) {
		const s = this.state, r = this.state.strokeStyle, o = this.state.lineWidth, a = this.state.strokeOffset;
		if (this.state.strokeStyle === "undefined" || this.state.lineWidth === "undefined") return;
		this.updateStrokeStyle(this.state, this.applyStroke);
		this.beginGeometry(e, t, i);
		this.hitDetectionInstructions.push([
			ks.SET_STROKE_STYLE,
			this.state.strokeStyle,
			this.state.lineWidth,
			this.state.lineCap,
			this.state.lineJoin,
			this.state.miterLimit,
			ti,
			ii
		], $i);
		const l = e.getEnds(), h = e.getFlatCoordinates(), c = e.getStride();
		let u = 0;
		for (let d = 0, f = l.length; d < f; ++d) u = this.drawFlatCoordinates_(h, u, l[d], c, this.state.strokeOffset);
		this.hitDetectionInstructions.push(xi);
		this.endGeometry(t);
	}
	finish() {
		const e = this.state;
		if (this.state.lastStroke != null && this.state.lastStroke != this.coordinates.length) {
			this.instructions.push(xi);
		}
		this.reverseHitDetectionInstructions();
		this.state = null;
		return super.finish();
	}
	applyStroke(e) {
		if (e.lastStroke != null && e.lastStroke != this.coordinates.length) {
			this.instructions.push(xi);
			e.lastStroke = this.coordinates.length;
		}
		e.lastStroke = 0;
		super.applyStroke(e);
		this.instructions.push($i);
	}
}
class lm extends nm {
	constructor(e, t, i, s) {
		super(e, t, i, s);
	}
	drawFlatCoordinatess_(e, t, i, s, r) {
		const o = this.state, a = this.state.fillStyle !== "undefined", l = this.state.strokeStyle !== "undefined", h = i.length;
		this.instructions.push($i);
		this.hitDetectionInstructions.push($i);
		for (let c = 0; c < i.length; ++c) {
			const u = i[c];
			const d = this.coordinates.length;
			const f = this.appendFlatLineCoordinates(e, t, i[c], s, true, !l);
			this.instructions.push([
				ks.MOVE_TO_LINE_TO,
				this.coordinates.length,
				f,
				r * this.pixelRatio,
				true
			]);
			this.hitDetectionInstructions.push([
				ks.MOVE_TO_LINE_TO,
				this.coordinates.length,
				f,
				r,
				true
			]);
			if (l) {
				this.instructions.push(oc);
				this.hitDetectionInstructions.push(oc);
			}
			t = i[c];
		}
		if (a) {
			this.instructions.push(ir);
			this.hitDetectionInstructions.push(ir);
		}
		if (l) {
			this.instructions.push(xi);
			this.hitDetectionInstructions.push(xi);
		}
		return t;
	}
	drawCircle(e, t, i) {
		const s = this.state, r = this.state.fillStyle, o = this.state.strokeStyle, a = this.state.strokeOffset;
		if (this.state.fillStyle === "undefined" && this.state.strokeStyle === "undefined" || this.handleStrokeOffset_(() => this.drawCircle(e, t, i))) return;
		this.setFillStrokeStyles_();
		this.beginGeometry(e, t, i);
		if (this.state.fillStyle !== "undefined") {
			this.hitDetectionInstructions.push([ks.SET_FILL_STYLE, Ve]);
		}
		if (this.state.strokeStyle !== "undefined") {
			this.hitDetectionInstructions.push([
				ks.SET_STROKE_STYLE,
				this.state.strokeStyle,
				this.state.lineWidth,
				this.state.lineCap,
				this.state.lineJoin,
				this.state.miterLimit,
				ti,
				ii
			]);
		}
		const l = e.getFlatCoordinates(), h = e.getStride(), c = this.coordinates.length;
		this.appendFlatLineCoordinates(l, 0, l.length, h, false, false);
		const u = [
			ks.CIRCLE,
			this.coordinates.length,
			this.state.strokeOffset
		];
		this.instructions.push($i, u);
		this.hitDetectionInstructions.push($i, u);
		if (this.state.fillStyle !== "undefined") {
			this.instructions.push(ir);
			this.hitDetectionInstructions.push(ir);
		}
		if (this.state.strokeStyle !== "undefined") {
			this.instructions.push(xi);
			this.hitDetectionInstructions.push(xi);
		}
		this.endGeometry(t);
	}
	drawPolygon(e, t, i) {
		const s = this.state, r = this.state.fillStyle, o = this.state.strokeStyle, a = this.state.strokeOffset;
		if (this.state.fillStyle === "undefined" && this.state.strokeStyle === "undefined" || this.handleStrokeOffset_(() => this.drawPolygon(e, t, i))) return;
		this.setFillStrokeStyles_();
		this.beginGeometry(e, t, i);
		if (this.state.fillStyle !== "undefined") {
			this.hitDetectionInstructions.push([ks.SET_FILL_STYLE, Ve]);
		}
		if (this.state.strokeStyle !== "undefined") {
			this.hitDetectionInstructions.push([
				ks.SET_STROKE_STYLE,
				this.state.strokeStyle,
				this.state.lineWidth,
				this.state.lineCap,
				this.state.lineJoin,
				this.state.miterLimit,
				ti,
				ii
			]);
		}
		const l = e.getEnds(), h = e.getOrientedFlatCoordinates(), c = e.getStride();
		this.drawFlatCoordinatess_(h, 0, l, c, this.state.strokeOffset);
		this.endGeometry(t);
	}
	drawMultiPolygon(e, t, i) {
		const s = this.state, r = this.state.fillStyle, o = this.state.strokeStyle, a = this.state.strokeOffset;
		if (this.state.fillStyle === "undefined" && this.state.strokeStyle === "undefined" || this.handleStrokeOffset_(() => this.drawMultiPolygon(e, t, i))) return;
		this.setFillStrokeStyles_();
		this.beginGeometry(e, t, i);
		if (this.state.fillStyle !== "undefined") {
			this.hitDetectionInstructions.push([ks.SET_FILL_STYLE, Ve]);
		}
		if (this.state.strokeStyle !== "undefined") {
			this.hitDetectionInstructions.push([
				ks.SET_STROKE_STYLE,
				this.state.strokeStyle,
				this.state.lineWidth,
				this.state.lineCap,
				this.state.lineJoin,
				this.state.miterLimit,
				ti,
				ii
			]);
		}
		const l = e.getEndss(), h = e.getOrientedFlatCoordinates(), c = e.getStride();
		let u = 0;
		for (let d = 0, f = l.length; d < f; ++d) u = this.drawFlatCoordinatess_(h, u, l[d], c, this.state.strokeOffset);
		this.endGeometry(t);
	}
	finish() {
		this.reverseHitDetectionInstructions();
		this.state = null;
		const e = this.tolerance;
		if (this.tolerance !== 0) {
			const t = this.coordinates;
			for (let i = 0, s = this.coordinates.length; i < s; ++i) this.coordinates[i] = Di(this.coordinates[i], e);
		}
		return super.finish();
	}
	setFillStrokeStyles_() {
		const e = this.state;
		this.updateFillStyle(this.state, this.createFill);
		this.updateStrokeStyle(this.state, this.applyStroke);
	}
	handleStrokeOffset_(e) {
		const t = this.state, i = this.state.fillStyle, s = this.state.strokeStyle, r = this.state.strokeOffset;
		return Math.abs(this.state.strokeOffset) > 0 && this.state.fillStyle !== "undefined" && this.state.strokeStyle !== "undefined" ? (this.state.strokeStyle = "undefined", this.state.strokeOffset = 0, e(), this.state.fillStyle = "undefined", this.state.strokeStyle = this.state.strokeStyle, this.state.strokeOffset = this.state.strokeOffset, e(), this.state.fillStyle = this.state.fillStyle, true) : false;
	}
}
function hm(n, e, t, i, s) {
	const r = [];
	let o = t, a = 0, l = e.slice(t, 2);
	for (; a < n && o + s < i;) {
		const [h, c] = l.slice(-2);
		const u = e[o + s];
		const d = e[o + s + 1];
		const f = Math.sqrt((e[o + s] - h) * (e[o + s] - h) + (e[o + s + 1] - c) * (e[o + s + 1] - c));
		a += f;
		if (a >= n) {
			const g = (n - a + f) / f;
			const m = Qe(h, u, g);
			const _ = Qe(c, d, g);
			l.push(m, _);
			r.push(l);
			l = [m, _];
			if (a == n) {
				o += s;
			}
			a = 0;
		} else if (a < n) {
			l.push(e[o + s], e[o + s + 1]);
			o += s;
		} else {
			const g = f - a;
			const m = Qe(h, u, g / f);
			const _ = Qe(c, d, g / f);
			l.push(m, _);
			r.push(l);
			l = [m, _];
			a = 0;
			o += s;
		}
	}
	if (a > 0) {
		r.push(l);
	}
	return r;
}
function cm(n, e, t, i, s) {
	let r = t, o = t, a = 0, l = 0, h = t, c, u, d, f, g, m, _, p, y, E;
	for (u = t; u < i; u += s) {
		const x = e[u];
		const T = e[u + 1];
		if (g !== "undefined") {
			y = e[u] - g;
			E = e[u + 1] - m;
			f = Math.sqrt(y * y + E * E);
			if (_ !== "undefined") {
				l += d;
				c = Math.acos((_ * y + p * E) / (d * f));
				if (c > n) {
					if (l > a) {
						a = l;
						r = h;
						o = u;
					}
					l = 0;
					h = u - s;
				}
			}
			d = f;
			_ = y;
			p = E;
		}
		g = e[u];
		m = e[u + 1];
	}
	l += f;
	return l > a ? [h, u] : [r, o];
}
const zr = {
	left: 0,
	center: .5,
	right: 1,
	top: 0,
	middle: .5,
	hanging: .2,
	alphabetic: .8,
	ideographic: .8,
	bottom: 1
};
class um extends nm {
	constructor(e, t, i, s) {
		super(e, t, i, s);
		this.labels_ = null;
		this.text_ = "";
		this.textOffsetX_ = 0;
		this.textOffsetY_ = 0;
		this.textRotateWithView_ = "undefined";
		this.textKeepUpright_ = "undefined";
		this.textRotation_ = 0;
		this.textFillState_ = null;
		this.fillStates = {};
		this.fillStates[Ve] = { fillStyle: Ve };
		this.textStrokeState_ = null;
		this.strokeStates = {};
		this.textState_ = {};
		this.textStates = {};
		this.textKey_ = "";
		this.fillKey_ = "";
		this.strokeKey_ = "";
		this.declutterMode_ = "undefined";
		this.declutterImageWithText_ = "undefined";
	}
	finish() {
		const e = super.finish();
		e.textStates = this.textStates;
		e.fillStates = this.fillStates;
		e.strokeStates = this.strokeStates;
		return e;
	}
	drawText(e, t, i) {
		const s = this.textFillState_, r = this.textStrokeState_, o = this.textState_;
		if (this.text_ === "" || !this.textState_ || !this.textFillState_ && !this.textStrokeState_) return;
		const a = this.coordinates;
		let l = this.coordinates.length;
		const h = e.getType();
		let c = null, u = e.getStride();
		if (this.textState_.placement === "line" && (h == "LineString" || h == "MultiLineString" || h == "Polygon" || h == "MultiPolygon")) {
			if (!me(this.maxExtent, e.getExtent())) return;
			let d;
			c = e.getFlatCoordinates();
			if (h == "LineString") d = [c.length];
			else if (h == "MultiLineString") d = e.getEnds();
			else if (h == "Polygon") d = e.getEnds().slice(0, 1);
			else if (h == "MultiPolygon") {
				const _ = e.getEndss();
				d = [];
				for (let p = 0, y = _.length; p < y; ++p) d.push(_[p][0]);
			}
			this.beginGeometry(e, t, i);
			const f = o.repeat;
			const g = o.repeat ? "undefined" : o.textAlign;
			let m = 0;
			for (let _ = 0, p = d.length; _ < p; ++_) {
				let y;
				f ? y = hm(f * this.resolution, c, m, d[_], u) : y = [c.slice(m, d[_])];
				for (let E = 0, x = y.length; E < x; ++E) {
					const T = y[E];
					let v = 0;
					let P = y[E].length;
					if (g == null) {
						const R = cm(o.maxAngle, T, 0, T.length, 2);
						v = R[0];
						P = R[1];
					}
					for (let R = v; R < P; R += u) a.push(y[E][R], y[E][R + 1]);
					const S = a.length;
					m = d[_];
					this.drawChars_(l, a.length);
					l = a.length;
				}
			}
			this.endGeometry(t);
		} else {
			let d = o.overflow ? null : [];
			switch (h) {
				case "Point":
				case "MultiPoint":
					c = e.getFlatCoordinates();
					break;
				case "LineString":
					c = e.getFlatMidpoint();
					break;
				case "Circle":
					c = e.getCenter();
					break;
				case "MultiLineString":
					c = e.getFlatMidpoints(), u = 2;
					break;
				case "Polygon":
					c = e.getFlatInteriorPoint(), o.overflow || d.push(c[2] / this.resolution), u = 3;
					break;
				case "MultiPolygon":
					const x = e.getFlatInteriorPoints();
					c = [];
					for (let T = 0, v = x.length; T < v; T += 3) {
						if (!o.overflow) {
							d.push(x[T + 2] / this.resolution);
						}
						c.push(x[T], x[T + 1]);
					}
					if (c.length === 0) return;
					u = 2;
					break;
			}
			const f = this.appendFlatPointCoordinates(c, u);
			if (f === l) return;
			if (d && (f - l) / 2 !== c.length / u) {
				let x = l / 2;
				d = d.filter((T, v) => {
					const P = a[(x + v) * 2] === c[v * u] && a[(x + v) * 2 + 1] === c[v * u + 1];
					if (!P) {
						--x;
					}
					return P;
				});
			}
			this.saveTextStates_();
			const g = o.backgroundFill ? this.createFill(this.fillStyleToState(o.backgroundFill)) : null;
			const m = o.backgroundStroke ? this.createStroke(this.strokeStyleToState(o.backgroundStroke)) : null;
			this.beginGeometry(e, t, i);
			let _ = o.padding;
			if (_ != Bi && (o.scale[0] < 0 || o.scale[1] < 0)) {
				let x = o.padding[0];
				let T = o.padding[1];
				let v = o.padding[2];
				let P = o.padding[3];
				if (o.scale[0] < 0) {
					T = -T;
					P = -P;
				}
				if (o.scale[1] < 0) {
					x = -x;
					v = -v;
				}
				_ = [
					x,
					T,
					v,
					P
				];
			}
			const p = this.pixelRatio;
			this.instructions.push([
				ks.DRAW_IMAGE,
				l,
				f,
				null,
				NaN,
				NaN,
				NaN,
				1,
				0,
				0,
				this.textRotateWithView_,
				this.textRotation_,
				[1, 1],
				NaN,
				this.declutterMode_,
				this.declutterImageWithText_,
				_ == Bi ? Bi : _.map(function(x) {
					return x * p;
				}),
				g,
				m,
				this.text_,
				this.textKey_,
				this.strokeKey_,
				this.fillKey_,
				this.textOffsetX_,
				this.textOffsetY_,
				d
			]);
			const y = 1 / this.pixelRatio;
			const E = g ? g.slice(0) : null;
			if (E) {
				E[1] = Ve;
			}
			this.hitDetectionInstructions.push([
				ks.DRAW_IMAGE,
				l,
				f,
				null,
				NaN,
				NaN,
				NaN,
				1,
				0,
				0,
				this.textRotateWithView_,
				this.textRotation_,
				[y, y],
				NaN,
				this.declutterMode_,
				this.declutterImageWithText_,
				_,
				E,
				m,
				this.text_,
				this.textKey_,
				this.strokeKey_,
				this.fillKey_ ? Ve : this.fillKey_,
				this.textOffsetX_,
				this.textOffsetY_,
				d
			]);
			this.endGeometry(t);
		}
	}
	saveTextStates_() {
		const e = this.textStrokeState_, t = this.textState_, i = this.textFillState_, s = this.strokeKey_;
		if (this.textStrokeState_) {
			if (!(this.strokeKey_ in this.strokeStates)) {
				this.strokeStates[this.strokeKey_] = {
					strokeStyle: this.textStrokeState_.strokeStyle,
					lineCap: this.textStrokeState_.lineCap,
					lineDashOffset: this.textStrokeState_.lineDashOffset,
					lineWidth: this.textStrokeState_.lineWidth,
					lineJoin: this.textStrokeState_.lineJoin,
					miterLimit: this.textStrokeState_.miterLimit,
					lineDash: this.textStrokeState_.lineDash
				};
			}
		}
		const r = this.textKey_;
		if (!(this.textKey_ in this.textStates)) {
			this.textStates[this.textKey_] = {
				font: this.textState_.font,
				textAlign: this.textState_.textAlign || Cs,
				justify: this.textState_.justify,
				textBaseline: this.textState_.textBaseline || Ur,
				scale: this.textState_.scale
			};
		}
		const o = this.fillKey_;
		if (this.textFillState_) {
			if (!(this.fillKey_ in this.fillStates)) {
				this.fillStates[this.fillKey_] = { fillStyle: this.textFillState_.fillStyle };
			}
		}
	}
	drawChars_(e, t) {
		const i = this.textStrokeState_, s = this.textState_, r = this.strokeKey_, o = this.textKey_, a = this.fillKey_;
		this.saveTextStates_();
		const l = this.pixelRatio, h = zr[this.textState_.textBaseline], c = this.textOffsetY_ * this.pixelRatio, u = this.text_, d = this.textStrokeState_ ? this.textStrokeState_.lineWidth * Math.abs(this.textState_.scale[0]) / 2 : 0;
		this.instructions.push([
			ks.DRAW_CHARS,
			e,
			t,
			zr[this.textState_.textBaseline],
			this.textState_.overflow,
			this.fillKey_,
			this.textState_.maxAngle,
			this.pixelRatio,
			c,
			this.strokeKey_,
			d * this.pixelRatio,
			this.text_,
			this.textKey_,
			1,
			this.declutterMode_,
			this.textKeepUpright_
		]);
		this.hitDetectionInstructions.push([
			ks.DRAW_CHARS,
			e,
			t,
			zr[this.textState_.textBaseline],
			this.textState_.overflow,
			this.fillKey_ && Ve,
			this.textState_.maxAngle,
			this.pixelRatio,
			c,
			this.strokeKey_,
			d * this.pixelRatio,
			this.text_,
			this.textKey_,
			1 / this.pixelRatio,
			this.declutterMode_,
			this.textKeepUpright_
		]);
	}
	setTextStyle(e, t) {
		let i, s, r;
		if (!e) this.text_ = "";
		else {
			const o = e.getFill();
			o ? (s = this.textFillState_, s || (s = {}, this.textFillState_ = s), s.fillStyle = It(o.getColor() || Ve)) : (s = null, this.textFillState_ = s);
			const a = e.getStroke();
			if (!a) {
				r = null;
				this.textStrokeState_ = r;
			} else {
				r = this.textStrokeState_;
				if (!r) {
					r = {};
					this.textStrokeState_ = r;
				}
				const m = a.getLineDash();
				const _ = a.getLineDashOffset();
				const p = a.getWidth();
				const y = a.getMiterLimit();
				r.lineCap = a.getLineCap() || In;
				r.lineDash = m ? m.slice() : ti;
				r.lineDashOffset = _ === "undefined" ? ii : _;
				r.lineJoin = a.getLineJoin() || Fn;
				r.lineWidth = p === "undefined" ? Rs : p;
				r.miterLimit = y === "undefined" ? Es : y;
				r.strokeStyle = It(a.getColor() || Ts);
			}
			i = this.textState_;
			const l = e.getFont() || Bu;
			N_(l);
			const h = e.getScaleArray();
			i.overflow = e.getOverflow();
			i.font = l;
			i.maxAngle = e.getMaxAngle();
			i.placement = e.getPlacement();
			i.textAlign = e.getTextAlign();
			i.repeat = e.getRepeat();
			i.justify = e.getJustify();
			i.textBaseline = e.getTextBaseline() || Ur;
			i.backgroundFill = e.getBackgroundFill();
			i.backgroundStroke = e.getBackgroundStroke();
			i.padding = e.getPadding() || Bi;
			i.scale = h === "undefined" ? [1, 1] : h;
			const c = e.getOffsetX();
			const u = e.getOffsetY();
			const d = e.getRotateWithView();
			const f = e.getKeepUpright();
			const g = e.getRotation();
			this.text_ = e.getText() || "";
			this.textOffsetX_ = c === "undefined" ? 0 : c;
			this.textOffsetY_ = u === "undefined" ? 0 : u;
			this.textRotateWithView_ = d === "undefined" ? false : d;
			this.textKeepUpright_ = f === "undefined" ? true : f;
			this.textRotation_ = g === "undefined" ? 0 : g;
			this.strokeKey_ = r ? (typeof r.strokeStyle == "string" ? r.strokeStyle : O(r.strokeStyle)) + r.lineCap + r.lineDashOffset + "|" + r.lineWidth + r.lineJoin + r.miterLimit + "[" + r.lineDash.join() + "]" : "";
			this.textKey_ = i.font + i.scale + (i.textAlign || "?") + (i.repeat || "?") + (i.justify || "?") + (i.textBaseline || "?");
			this.fillKey_ = s && s.fillStyle ? typeof s.fillStyle == "string" ? s.fillStyle : "|" + O(s.fillStyle) : "";
		}
		this.declutterMode_ = e.getDeclutterMode();
		this.declutterImageWithText_ = t;
	}
}
const dm = {
	Circle: lm,
	Default: nm,
	Image: sm,
	LineString: om,
	Polygon: lm,
	Text: um
};
class fm {
	constructor(e, t, i, s) {
		this.tolerance_ = e;
		this.maxExtent_ = t;
		this.pixelRatio_ = s;
		this.resolution_ = i;
		this.buildersByZIndex_ = {};
	}
	finish() {
		const e = {};
		for (const t in this.buildersByZIndex_) {
			e[t] = e[t] || {};
			const i = this.buildersByZIndex_[t];
			for (const s in this.buildersByZIndex_[t]) {
				const r = i[s].finish();
				e[t][s] = r;
			}
		}
		return e;
	}
	getBuilder(e, t) {
		const i = e !== "undefined" ? e.toString() : "0";
		let s = this.buildersByZIndex_[i];
		if (s === "undefined") {
			s = {};
			this.buildersByZIndex_[i] = s;
		}
		let r = s[t];
		if (r === "undefined") {
			const o = dm[t];
			r = new dm[t](this.tolerance_, this.maxExtent_, this.resolution_, this.pixelRatio_);
			s[t] = r;
		}
		return r;
	}
}
function gm(n, e, t, i, s, r, o, a, l, h, c, u, d = true) {
	let f = n[e], g = n[e + 1], m = 0, _ = 0, p = 0, y = 0;
	function E() {
		m = f;
		_ = g;
		e += i;
		f = n[e];
		g = n[e + 1];
		y += p;
		p = Math.sqrt((f - m) * (f - m) + (g - _) * (g - _));
	}
	do
		E();
	while (e < t - i && y + p < r);
	let x = p === 0 ? 0 : (r - y) / p;
	const T = Qe(m, f, x), v = Qe(_, g, x), P = e - i, S = y, R = r + a * l(h, s, c);
	for (; e < t - i && y + p < R;) E();
	x = p === 0 ? 0 : (R - y) / p;
	const I = Qe(m, f, x), N = Qe(_, g, x);
	let L = false;
	if (d) if (u) {
		const D = [
			T,
			v,
			I,
			N
		];
		cl(D, 0, 4, 2, u, D, D);
		L = D[0] > D[2];
	} else L = T > I;
	const A = Math.PI, W = [], w = P + i === e;
	e = P;
	p = 0;
	y = y;
	f = n[e];
	g = n[e + 1];
	let b;
	if (w) {
		E();
		b = Math.atan2(g - _, f - m);
		if (L) {
			b += b > 0 ? -A : A;
		}
		const D = (I + T) / 2;
		const k = (N + v) / 2;
		W[0] = [
			D,
			k,
			(R - r) / 2,
			b,
			s
		];
		return W;
	}
	s = s.replace(/\n/g, " ");
	for (let D = 0, k = s.length; D < k;) {
		E();
		let B = Math.atan2(g - _, f - m);
		if (L) {
			B += B > 0 ? -A : A;
		}
		if (b !== "undefined") {
			let ve = B - b;
			ve += ve > A ? -2 * A : ve < -A ? 2 * A : 0;
			if (Math.abs(ve) > o) return null;
		}
		b = B;
		const q = D;
		let ie = 0;
		for (; D < k; ++D) {
			const ve = L ? k - D - 1 : D;
			const ze = a * l(h, s[ve], c);
			if (e + i < t && y + p < r + ie + ze / 2) break;
			ie += ze;
		}
		if (D === D) continue;
		const re = L ? s.substring(k - D, k - D) : s.substring(D, D);
		x = p === 0 ? 0 : (r + ie / 2 - y) / p;
		const ce = Qe(m, f, x);
		const Fe = Qe(_, g, x);
		W.push([
			ce,
			Fe,
			ie / 2,
			B,
			re
		]);
		r += ie;
	}
	return W;
}
class _m {
	constructor() {
		Fo(this, "pushMethodArgs_", (...e) => this.push_(e));
		this.instructions_ = [];
		this.zIndex = 0;
		this.offset_ = 0;
		this.context_ = new Proxy($r(), {
			get: (e, t) => {
				if (typeof $r()[t] == "function") return this.push_(t), this.pushMethodArgs_;
			},
			set: (e, t, i) => this.push_(t, i)
		});
	}
	push_(...e) {
		const t = this.instructions_, i = this.zIndex + this.offset_;
		if (!this.instructions_[i]) {
			this.instructions_[i] = [];
		}
		this.instructions_[i].push(...e);
	}
	pushFunction(e) {
		this.push_(e);
	}
	getContext() {
		return this.context_;
	}
	draw(e) {
		this.instructions_.forEach((t) => {
			for (let i = 0, s = t.length; i < s; ++i) {
				const r = t[i];
				if (typeof t[i] == "function") {
					r(e);
					continue;
				}
				const o = t[++i];
				if (typeof e[t[i]] == "function") e[t[i]](...t[++i]);
				else {
					if (typeof o == "function") {
						e[r] = o(e);
						continue;
					}
					e[r] = o;
				}
			}
		});
	}
	clear() {
		this.instructions_.length = 0;
		this.zIndex = 0;
		this.offset_ = 0;
	}
	offset() {
		this.offset_ = this.instructions_.length;
		this.zIndex = 0;
	}
}
const Qi = je();
const ci = [];
const jt = [];
const zt = [];
const ui = [];
function lc(n) {
	return n[3].declutterBox;
}
const hc = new RegExp("[֑-ࣿיִ-﷿ﹰ-ﻼࠀ-࿿-]");
function Wo(n, e) {
	e === "start" ? e = hc.test(n) ? "right" : "left" : e === "end" && (e = hc.test(n) ? "left" : "right");
	return zr[e];
}
function mm(n, e, t) {
	if (t > 0) {
		n.push("\n", "");
	}
	n.push(e, "");
	return n;
}
function pm(n, e, t) {
	if (t % 2 === 0) {
		n += e;
	}
	return n;
}
class ym {
	constructor(e, t, i, s, r) {
		this.overlaps = i;
		this.pixelRatio = t;
		this.resolution = e;
		this.alignAndScaleFill_;
		this.instructions = s.instructions;
		this.coordinates = s.coordinates;
		this.coordinateCache_ = {};
		this.renderedTransform_ = Se();
		this.hitDetectionInstructions = s.hitDetectionInstructions;
		this.pixelCoordinates_ = null;
		this.viewRotation_ = 0;
		this.fillStates = s.fillStates || {};
		this.strokeStates = s.strokeStates || {};
		this.textStates = s.textStates || {};
		this.widths_ = {};
		this.labels_ = {};
		this.zIndexContext_ = r ? new _m() : null;
	}
	getZIndexContext() {
		return this.zIndexContext_;
	}
	createLabel(e, t, i, s) {
		const r = e + t + i + s;
		if (this.labels_[r]) return this.labels_[r];
		const o = s ? this.strokeStates[s] : null, a = i ? this.fillStates[i] : null, l = this.textStates[t], h = this.pixelRatio, c = [this.textStates[t].scale[0] * this.pixelRatio, this.textStates[t].scale[1] * this.pixelRatio], u = this.textStates[t].justify ? zr[this.textStates[t].justify] : Wo(Array.isArray(e) ? e[0] : e, this.textStates[t].textAlign || Cs), d = s && o.lineWidth ? o.lineWidth : 0, f = Array.isArray(e) ? e : String(e).split("\n").reduce(mm, []), { width: g, height: m, widths: _, heights: p, lineWidths: y } = G_(this.textStates[t], f), E = g + d, x = [], T = (E + 2) * c[0], v = (m + d) * c[1], P = {
			width: T < 0 ? Math.floor(T) : Math.ceil(T),
			height: v < 0 ? Math.floor(v) : Math.ceil(v),
			contextInstructions: x
		};
		if (c[0] != 1 || c[1] != 1) {
			x.push("scale", c);
		}
		if (s) {
			x.push("strokeStyle", o.strokeStyle);
			x.push("lineWidth", d);
			x.push("lineCap", o.lineCap);
			x.push("lineJoin", o.lineJoin);
			x.push("miterLimit", o.miterLimit);
			x.push("setLineDash", [o.lineDash]);
			x.push("lineDashOffset", o.lineDashOffset);
		}
		if (i) {
			x.push("fillStyle", a.fillStyle);
		}
		x.push("textBaseline", "middle");
		x.push("textAlign", "center");
		const S = .5 - u;
		let R = u * E + S * d;
		const I = [], N = [];
		let L = 0, A = 0, W = 0, w = 0, b;
		for (let D = 0, k = f.length; D < k; D += 2) {
			const B = f[D];
			if (f[D] === "\n") {
				A += L;
				L = 0;
				R = u * E + S * d;
				++w;
				continue;
			}
			const q = f[D + 1] || l.font;
			if (q !== b) {
				if (s) {
					I.push("font", q);
				}
				if (i) {
					N.push("font", q);
				}
				b = q;
			}
			L = Math.max(L, p[W]);
			const ie = [
				f[D],
				R + S * _[W] + u * (_[W] - y[w]),
				.5 * (d + L) + A
			];
			R += _[W];
			if (s) {
				I.push("strokeText", ie);
			}
			if (i) {
				N.push("fillText", ie);
			}
			++W;
		}
		Array.prototype.push.apply(x, I);
		Array.prototype.push.apply(x, N);
		this.labels_[r] = P;
		return P;
	}
	replayTextBackground_(e, t, i, s, r, o, a) {
		e.beginPath();
		e.moveTo.apply(e, t);
		e.lineTo.apply(e, i);
		e.lineTo.apply(e, s);
		e.lineTo.apply(e, r);
		e.lineTo.apply(e, t);
		if (o) {
			this.alignAndScaleFill_ = o[2];
			e.fillStyle = o[1];
			this.fill_(e);
		}
		if (a) {
			this.setStrokeStyle_(e, a);
			e.stroke();
		}
	}
	calculateImageOrLabelDimensions_(e, t, i, s, r, o, a, l, h, c, u, d, f, g, m, _) {
		a *= d[0];
		l *= d[1];
		let p = i - a, y = s - l;
		const E = r + h > e ? e - h : r, x = o + c > t ? t - c : o, T = g[3] + E * d[0] + g[1], v = g[0] + x * d[1] + g[2], P = p - g[3], S = y - g[0];
		if (m || u !== 0) {
			ci[0] = P;
			ui[0] = P;
			ci[1] = S;
			jt[1] = S;
			jt[0] = P + T;
			zt[0] = jt[0];
			zt[1] = S + v;
			ui[1] = zt[1];
		}
		let R;
		u !== 0 ? (R = gt(Se(), i, s, 1, 1, u, -i, -s), xe(R, ci), xe(R, jt), xe(R, zt), xe(R, ui), bt(Math.min(ci[0], jt[0], zt[0], ui[0]), Math.min(ci[1], jt[1], zt[1], ui[1]), Math.max(ci[0], jt[0], zt[0], ui[0]), Math.max(ci[1], jt[1], zt[1], ui[1]), Qi)) : bt(Math.min(P, P + T), Math.min(S, S + v), Math.max(P, P + T), Math.max(S, S + v), Qi);
		if (f) {
			p = Math.round(p);
			y = Math.round(y);
		}
		return {
			drawImageX: p,
			drawImageY: y,
			drawImageW: E,
			drawImageH: x,
			originX: h,
			originY: c,
			declutterBox: {
				minX: Qi[0],
				minY: Qi[1],
				maxX: Qi[2],
				maxY: Qi[3],
				value: _
			},
			canvasTransform: R,
			scale: d
		};
	}
	replayImageOrLabel_(e, t, i, s, r, o, a) {
		const l = !!(o || a), h = s.declutterBox, c = a ? a[2] * s.scale[0] / 2 : 0;
		if (s.declutterBox.minX - c <= t[0] && s.declutterBox.maxX + c >= 0 && s.declutterBox.minY - c <= t[1] && s.declutterBox.maxY + c >= 0) {
			if (l) {
				this.replayTextBackground_(e, ci, jt, zt, ui, o, a);
			}
			B_(e, s.canvasTransform, r, i, s.originX, s.originY, s.drawImageW, s.drawImageH, s.drawImageX, s.drawImageY, s.scale);
		}
		return true;
	}
	fill_(e) {
		const t = this.alignAndScaleFill_;
		if (this.alignAndScaleFill_) {
			const i = xe(this.renderedTransform_, [0, 0]);
			const s = 512 * this.pixelRatio;
			e.save();
			e.translate(i[0] % s, i[1] % s);
			if (t !== 1) {
				e.scale(t, t);
			}
		}
		e.fill();
		if (this.alignAndScaleFill_) {
			e.restore();
		}
	}
	setStrokeStyle_(e, t) {
		e.strokeStyle = t[1];
		if (t[1]) {
			e.lineWidth = t[2];
			e.lineCap = t[3];
			e.lineJoin = t[4];
			e.miterLimit = t[5];
			e.lineDashOffset = t[7];
			e.setLineDash(t[6]);
		}
	}
	drawLabelWithPointPlacement_(e, t, i, s) {
		const r = this.textStates[t], o = this.createLabel(e, t, s, i), a = this.strokeStates[i], l = this.pixelRatio, h = Wo(Array.isArray(e) ? e[0] : e, this.textStates[t].textAlign || Cs), c = zr[this.textStates[t].textBaseline || Ur], u = this.strokeStates[i] && this.strokeStates[i].lineWidth ? this.strokeStates[i].lineWidth : 0, d = o.width / this.pixelRatio - 2 * this.textStates[t].scale[0], f = h * d + 2 * (.5 - h) * u, g = zr[this.textStates[t].textBaseline || Ur] * o.height / this.pixelRatio + 2 * (.5 - zr[this.textStates[t].textBaseline || Ur]) * u;
		return {
			label: o,
			anchorX: f,
			anchorY: g
		};
	}
	execute_(e, t, i, s, r, o, a, l) {
		var ve, ze;
		const h = this.zIndexContext_;
		let c;
		this.pixelCoordinates_ && kt(i, this.renderedTransform_) ? c = this.pixelCoordinates_ : (this.pixelCoordinates_ || (this.pixelCoordinates_ = []), c = At(this.coordinates, 0, this.coordinates.length, 2, i, this.pixelCoordinates_), _u(this.renderedTransform_, i));
		let u = 0;
		const d = s.length;
		let f = 0, g, m, _, p, y, E, x, T, v, P, S, R, I, N, L, A, W = 0, w = 0;
		const b = this.coordinateCache_, D = this.viewRotation_, k = Math.round(Math.atan2(-i[1], i[0]) * 0xe8d4a51000) / 0xe8d4a51000, B = {
			context: e,
			pixelRatio: this.pixelRatio,
			resolution: this.resolution,
			rotation: this.viewRotation_
		}, q = this.instructions != s || this.overlaps ? 0 : 200;
		let ie, re, ce, Fe;
		for (; u < s.length;) {
			const F = s[u];
			switch (s[u][0]) {
				case ks.BEGIN_GEOMETRY:
					ie = s[u][1], Fe = s[u][3], ie.getGeometry() ? a !== "undefined" && !me(a, Fe.getExtent()) ? u = s[u][2] + 1 : ++u : u = s[u][2], h && (h.zIndex = s[u][4]);
					break;
				case ks.BEGIN_PATH:
					W > q && (this.fill_(e), W = 0), w > q && (e.stroke(), w = 0), !W && !w && (e.beginPath(), T = NaN, v = NaN), ++u;
					break;
				case ks.CIRCLE:
					f = s[u][1], p = (ve = s[u][2]) != null ? ve : 0;
					const it = c[f], Xe = c[f + 1], Xs = c[f + 2] - p, To = c[f + 3] - p, Xn = Xs - it, Ws = To - Xe, mt = Math.sqrt(Xn * Xn + Ws * Ws);
					e.moveTo(it + mt, Xe), e.arc(it, Xe, mt, 0, 2 * Math.PI, true), ++u;
					break;
				case ks.CLOSE_PATH:
					e.closePath(), ++u;
					break;
				case ks.CUSTOM:
					f = s[u][1], g = s[u][2];
					const St = s[u][3], Ki = s[u][4], Li = s[u][5];
					B.geometry = St, B.feature = ie, u in b || (b[u] = []);
					const li = b[u];
					Li ? Li(c, f, g, 2, li) : (li[0] = c[f], li[1] = c[f + 1], li.length = 2), h && (h.zIndex = s[u][6]), Ki(li, B), ++u;
					break;
				case ks.DRAW_IMAGE:
					f = s[u][1], g = s[u][2], R = s[u][3], m = s[u][4], _ = s[u][5];
					let Ai = s[u][6];
					const Hi = s[u][7], Co = s[u][8], We = s[u][9], Ch = s[u][10];
					let Ro = s[u][11];
					const cf = s[u][12];
					let Vs = s[u][13];
					x = s[u][14] || "declutter";
					const Wn = s[u][15];
					if (!R && s[u].length >= 20) {
						I = F[19];
						N = F[20];
						L = F[21];
						A = F[22];
						const nt = this.drawLabelWithPointPlacement_(I, N, L, A);
						R = nt.label;
						F[3] = R;
						const Mi = F[23];
						m = (nt.anchorX - F[23]) * this.pixelRatio;
						F[4] = m;
						const st = F[24];
						_ = (nt.anchorY - F[24]) * this.pixelRatio;
						F[5] = _;
						Ai = R.height;
						F[6] = Ai;
						Vs = R.width;
						F[13] = Vs;
					}
					let So;
					s[u].length > 25 && (So = s[u][25]);
					let vo, Ys, Zs;
					s[u].length > 17 ? (vo = s[u][16], Ys = s[u][17], Zs = s[u][18]) : (vo = Bi, Ys = null, Zs = null), Ch && k ? Ro += D : !Ch && !k && (Ro -= D);
					let uf = 0;
					for (; f < g; f += 2) {
						if (So && So[uf++] < Vs / this.pixelRatio) continue;
						const nt = this.calculateImageOrLabelDimensions_(R.width, R.height, c[f], c[f + 1], Vs, Ai, m, _, Co, We, Ro, cf, r, vo, !!Ys || !!Zs, ie);
						const Mi = [
							e,
							t,
							R,
							nt,
							Hi,
							Ys,
							Zs
						];
						if (l) {
							let st;
							let vt;
							let rt;
							if (Wn) {
								const Ee = g - f;
								if (!Wn[Ee]) {
									Wn[Ee] = {
										args: Mi,
										declutterMode: x
									};
									continue;
								}
								const Be = Wn[Ee];
								st = Wn[Ee].args;
								vt = Wn[Ee].declutterMode;
								delete Wn[Ee];
								rt = lc(st);
							}
							let Bt;
							let $t;
							if (st && (vt !== "declutter" || !l.collides(rt))) {
								Bt = true;
							}
							if (x !== "declutter" || !l.collides(nt.declutterBox)) {
								$t = true;
							}
							if (vt === "declutter" && x === "declutter") {
								const Ee = Bt && $t;
								Bt = Ee;
								$t = Ee;
							}
							if (Bt) {
								if (vt !== "none") {
									l.insert(rt);
								}
								this.replayImageOrLabel_();
							}
							if ($t) {
								if (x !== "none") {
									l.insert(nt.declutterBox);
								}
								this.replayImageOrLabel_();
							}
						} else this.replayImageOrLabel_();
					}
					++u;
					break;
				case ks.DRAW_CHARS:
					const Rh = s[u][1], Sh = s[u][2], wo = s[u][3], df = s[u][4];
					A = s[u][5];
					const ff = s[u][6], vh = s[u][7], wh = s[u][8];
					L = s[u][9];
					const Po = s[u][10];
					I = s[u][11], Array.isArray(I) && (I = I.reduce(pm, "")), N = s[u][12];
					const Ph = [s[u][13], s[u][13]];
					x = s[u][14] || "declutter";
					const gf = s[u][15], Io = this.textStates[N], Vn = Io.font, Yn = [Io.scale[0] * vh, Io.scale[1] * vh];
					let Zn;
					Vn in this.widths_ ? Zn = this.widths_[Vn] : (Zn = {}, this.widths_[Vn] = Zn);
					const Ih = ml(c, Rh, Sh, 2), Fh = Math.abs(Yn[0]) * nc(Vn, I, Zn);
					if (df || Fh <= Ih) {
						const nt = this.textStates[N].textAlign;
						const Mi = (Ih - Fh) * Wo(I, this.textStates[N].textAlign);
						const st = gm(c, Rh, Sh, 2, I, Mi, ff, Math.abs(Yn[0]), nc, Vn, Zn, k ? 0 : this.viewRotation_, gf);
						e: if (st) {
							const vt = [];
							let rt;
							let Bt;
							let $t;
							let Ee;
							let Be;
							if (L) for (rt = 0, Bt = st.length; rt < Bt; ++rt) {
								Be = st[rt];
								$t = Be[4];
								Ee = this.createLabel($t, N, "", L);
								m = Be[2] + (Yn[0] < 0 ? -Po : Po);
								_ = wo * Ee.height + (.5 - wo) * 2 * Po * Yn[1] / Yn[0] - wh;
								const Ut = this.calculateImageOrLabelDimensions_(Ee.width, Ee.height, Be[0], Be[1], Ee.width, Ee.height, m, _, 0, 0, Be[3], Ph, false, Bi, false, ie);
								if (l && x === "declutter" && l.collides(Ut.declutterBox)) break e;
								vt.push([
									e,
									t,
									Ee,
									Ut,
									1,
									null,
									null
								]);
							}
							if (A) for (rt = 0, Bt = st.length; rt < Bt; ++rt) {
								Be = st[rt];
								$t = Be[4];
								Ee = this.createLabel($t, N, A, "");
								m = Be[2];
								_ = wo * Ee.height - wh;
								const Ut = this.calculateImageOrLabelDimensions_(Ee.width, Ee.height, Be[0], Be[1], Ee.width, Ee.height, m, _, 0, 0, Be[3], Ph, false, Bi, false, ie);
								if (l && x === "declutter" && l.collides(Ut.declutterBox)) break e;
								vt.push([
									e,
									t,
									Ee,
									Ut,
									1,
									null,
									null
								]);
							}
							if (l && x !== "none") {
								l.load(vt.map(lc));
							}
							for (let Ut = 0, _f = vt.length; Ut < _f; ++Ut) this.replayImageOrLabel_();
						}
					}
					++u;
					break;
				case ks.END_GEOMETRY:
					if (o !== "undefined") {
						ie = F[1];
						const nt = o(ie, Fe, x);
						if (nt) return nt;
					}
					++u;
					break;
				case ks.FILL:
					q ? W++ : this.fill_(e), ++u;
					break;
				case ks.MOVE_TO_LINE_TO:
					for (f = s[u][1], g = s[u][2], p = s[u][3], y = (ze = s[u][4]) != null ? ze : false, re = c[f], ce = c[f + 1], p && (E = f, [re, ce] = Tr(re, ce, y ? c[g - 4] : "undefined", y ? c[g - 3] : "undefined", c[f + 2], c[f + 3], p)), e.moveTo(re, ce), T = re + .5 | 0, v = ce + .5 | 0, f += 2; f < g; f += 2) {
						re = c[f];
						ce = c[f + 1];
						P = re + .5 | 0;
						S = ce + .5 | 0;
						if (f == g - 2 || P !== T || S !== v) {
							if (p) {
								f == g - 2 ? [re, ce] = Tr(re, ce, c[f - 2], c[f - 1], y ? c[E + 2] : "undefined", y ? c[E + 3] : "undefined", p) : [re, ce] = Tr(re, ce, c[f - 2], c[f - 1], c[f + 2], c[f + 3], p);
							}
							e.lineTo(re, ce);
							T = P;
							v = S;
						}
					}
					++u;
					break;
				case ks.SET_FILL_STYLE:
					this.alignAndScaleFill_ = s[u][2], W && (this.fill_(e), W = 0, w && (e.stroke(), w = 0)), e.fillStyle = s[u][1], ++u;
					break;
				case ks.SET_STROKE_STYLE:
					w && (e.stroke(), w = 0), this.setStrokeStyle_(e, s[u]), ++u;
					break;
				case ks.STROKE:
					q ? w++ : e.stroke(), ++u;
					break;
				default:
					++u;
					break;
			}
		}
	}
	execute(e, t, i, s, r, o) {
		this.viewRotation_ = s;
		this.execute_(e, t, i, this.instructions, r, "undefined", "undefined", o);
	}
	executeHitDetection(e, t, i, s, r) {
		this.viewRotation_ = i;
		return this.execute_(e, [e.canvas.width, e.canvas.height], t, this.hitDetectionInstructions, true, s, r);
	}
}
const Ni = [
	"Polygon",
	"Circle",
	"LineString",
	"Image",
	"Text",
	"Default"
];
const Xr = ["Image", "Text"];
const Em = Ni.filter((n) => !Xr.includes(n));
let Hu = false;
function Tm() {
	let n = 0;
	const e = (i) => {
		const s = Re(1, 1, null, { willReadFrequently: i });
		let r = 0;
		const o = performance.now();
		for (; performance.now() - o < 50; ++r) {
			s.fillStyle = "rgba(255,0,".concat(r % 256, ",1)");
			s.fillRect(0, 0, 1, 1);
			s.getImageData(0, 0, 1, 1);
		}
		n = r > n ? r : n;
		return r;
	};
	Hu = {
		[e(true)]: true,
		[e(false)]: false,
		[e("undefined")]: "undefined"
	}[n];
}
class Cm {
	constructor(e, t, i, s, r, o, a) {
		this.maxExtent_ = e;
		this.overlaps_ = s;
		this.pixelRatio_ = i;
		this.resolution_ = t;
		this.renderBuffer_ = o;
		this.executorsByZIndex_ = {};
		this.hitDetectionContext_ = null;
		this.hitDetectionTransform_ = Se();
		this.renderedContext_ = null;
		this.deferredZIndexContexts_ = {};
		this.createExecutors_(r, a);
	}
	clip(e, t) {
		const i = this.getClipCoords(t);
		e.beginPath();
		e.moveTo(i[0], i[1]);
		e.lineTo(i[2], i[3]);
		e.lineTo(i[4], i[5]);
		e.lineTo(i[6], i[7]);
		e.clip();
	}
	createExecutors_(e, t) {
		for (const i in e) {
			let s = this.executorsByZIndex_[i];
			if (s === "undefined") {
				s = {};
				this.executorsByZIndex_[i] = s;
			}
			const r = e[i];
			for (const o in e[i]) {
				const a = r[o];
				s[o] = new ym(this.resolution_, this.pixelRatio_, this.overlaps_, r[o], t);
			}
		}
	}
	hasExecutors(e) {
		for (const t in this.executorsByZIndex_) {
			const i = this.executorsByZIndex_[t];
			for (let s = 0, r = e.length; s < r; ++s) if (e[s] in this.executorsByZIndex_[t]) return true;
		}
		return false;
	}
	forEachFeatureAtCoordinate(e, t, i, s, r, o) {
		Tm();
		s = Math.round(s);
		const a = s * 2 + 1, l = gt(this.hitDetectionTransform_, s + .5, s + .5, 1 / t, -1 / t, -i, -e[0], -e[1]), h = !this.hitDetectionContext_;
		if (h) {
			this.hitDetectionContext_ = Re(a, a, null, { willReadFrequently: Hu });
		}
		const c = this.hitDetectionContext_;
		this.hitDetectionContext_.canvas.width !== a || this.hitDetectionContext_.canvas.height !== a ? (this.hitDetectionContext_.canvas.width = a, this.hitDetectionContext_.canvas.height = a) : h || this.hitDetectionContext_.clearRect(0, 0, a, a);
		let u;
		if (this.renderBuffer_ !== "undefined") {
			u = je();
			os(u, e);
			tt(u, t * (this.renderBuffer_ + s), u);
		}
		const d = Rm(s);
		let f;
		function g(T, v, P) {
			const S = c.getImageData(0, 0, a, a).data;
			for (let R = 0, I = d.length; R < I; R++) if (c.getImageData(0, 0, a, a).data[d[R]] > 0) {
				if (!o || P === "none" || f !== "Image" && f !== "Text" || o.includes(T)) {
					const N = (d[R] - 3) / 4;
					const L = s - N % a;
					const A = s - (N / a | 0);
					const W = r(T, v, L * L + A * A);
					if (W) return W;
				}
				c.clearRect(0, 0, a, a);
				break;
			}
		}
		const m = Object.keys(this.executorsByZIndex_).map(Number);
		m.sort(Lt);
		let _, p, y, E, x;
		for (_ = m.length - 1; _ >= 0; --_) {
			const T = m[_].toString();
			for (y = this.executorsByZIndex_[T], p = Ni.length - 1; p >= 0; --p) if (f = Ni[p], E = y[f], E !== "undefined" && (x = E.executeHitDetection(c, l, i, g, u), x)) return x;
		}
	}
	getClipCoords(e) {
		const t = this.maxExtent_;
		if (!this.maxExtent_) return null;
		const i = this.maxExtent_[0], s = this.maxExtent_[1], r = this.maxExtent_[2], o = this.maxExtent_[3], a = [
			this.maxExtent_[0],
			this.maxExtent_[1],
			this.maxExtent_[0],
			this.maxExtent_[3],
			this.maxExtent_[2],
			this.maxExtent_[3],
			this.maxExtent_[2],
			this.maxExtent_[1]
		];
		At(a, 0, 8, 2, e, a);
		return a;
	}
	isEmpty() {
		return si(this.executorsByZIndex_);
	}
	execute(e, t, i, s, r, o, a) {
		const l = Object.keys(this.executorsByZIndex_).map(Number);
		l.sort(a ? Rf : Lt);
		o = o || Ni;
		const h = Ni.length;
		for (let c = 0, u = l.length; c < u; ++c) {
			const d = l[c].toString();
			const f = this.executorsByZIndex_[d];
			for (let g = 0, m = o.length; g < m; ++g) {
				const _ = o[g];
				const p = f[o[g]];
				if (f[o[g]] !== "undefined") {
					const y = a === null ? "undefined" : p.getZIndexContext();
					const E = y ? y.getContext() : e;
					const x = this.maxExtent_ && _ !== "Image" && _ !== "Text";
					if (x) {
						E.save();
						this.clip(E, i);
					}
					!y || _ === "Text" || _ === "Image" ? p.execute(E, t, i, s, r, a) : y.pushFunction((T) => p.execute(T, t, i, s, r, a));
					if (x) {
						E.restore();
					}
					if (y) {
						y.offset();
						const T = l[c] * h + Ni.indexOf(_);
						if (!this.deferredZIndexContexts_[T]) {
							this.deferredZIndexContexts_[T] = [];
						}
						this.deferredZIndexContexts_[T].push(y);
					}
				}
			}
		}
		this.renderedContext_ = e;
	}
	getDeferredZIndexContexts() {
		return this.deferredZIndexContexts_;
	}
	getRenderedContext() {
		return this.renderedContext_;
	}
	renderDeferred() {
		const e = this.deferredZIndexContexts_, t = Object.keys(this.deferredZIndexContexts_).map(Number).sort(Lt);
		for (let i = 0, s = t.length; i < s; ++i) {
			this.deferredZIndexContexts_[t[i]].forEach((r) => {
				r.draw(this.renderedContext_);
				r.clear();
			});
			this.deferredZIndexContexts_[t[i]].length = 0;
		}
	}
}
const Vo = {};
function Rm(n) {
	if (Vo[n] !== "undefined") return Vo[n];
	const e = n * 2 + 1, t = n * n, i = new Array(t + 1);
	for (let r = 0; r <= n; ++r) for (let o = 0; o <= n; ++o) {
		const a = r * r + o * o;
		if (a > t) break;
		let l = i[a];
		if (!l) {
			l = [];
			i[a] = l;
		}
		l.push(((n + r) * e + (n + o)) * 4 + 3);
		if (r > 0) {
			l.push(((n - r) * e + (n + o)) * 4 + 3);
		}
		if (o > 0) {
			l.push(((n + r) * e + (n - o)) * 4 + 3);
			if (r > 0) {
				l.push(((n - r) * e + (n - o)) * 4 + 3);
			}
		}
	}
	const s = [];
	for (let r = 0, o = i.length; r < o; ++r) i[r] && s.push(...i[r]);
	Vo[n] = s;
	return s;
}
function cc(n, e, t, i) {
	return t !== "undefined" && i !== "undefined" ? [t / n, i / e] : t !== "undefined" ? t / n : i !== "undefined" ? i / e : 1;
}
class Nl extends Fl {
	constructor(e) {
		e = e || {};
		const t = e.opacity !== "undefined" ? e.opacity : 1, i = e.rotation !== "undefined" ? e.rotation : 0, s = e.scale !== "undefined" ? e.scale : 1, r = e.rotateWithView !== "undefined" ? e.rotateWithView : false;
		super({
			opacity: t,
			rotation: i,
			scale: s,
			displacement: e.displacement !== "undefined" ? e.displacement : [0, 0],
			rotateWithView: r,
			declutterMode: e.declutterMode
		});
		this.anchor_ = e.anchor !== "undefined" ? e.anchor : [.5, .5];
		this.normalizedAnchor_ = null;
		this.anchorOrigin_ = e.anchorOrigin !== "undefined" ? e.anchorOrigin : "top-left";
		this.anchorXUnits_ = e.anchorXUnits !== "undefined" ? e.anchorXUnits : "fraction";
		this.anchorYUnits_ = e.anchorYUnits !== "undefined" ? e.anchorYUnits : "fraction";
		this.crossOrigin_ = e.crossOrigin !== "undefined" ? e.crossOrigin : null;
		this.referrerPolicy_ = e.referrerPolicy;
		const o = e.img !== "undefined" ? e.img : null;
		let a = e.src;
		ee(!(a !== "undefined" && o), "`image` and `src` cannot be provided at the same time");
		if ((a === "undefined" || a.length === 0) && o) {
			a = o.src || O(o);
		}
		ee(a !== "undefined" && a.length > 0, "A defined and non-empty `src` or `image` must be provided");
		ee(!((e.width !== "undefined" || e.height !== "undefined") && e.scale !== "undefined"), "`width` or `height` cannot be provided together with `scale`");
		let l;
		e.src !== "undefined" ? l = $.IDLE : o !== "undefined" && ("complete" in o ? o.complete ? l = o.src ? $.LOADED : $.IDLE : l = $.LOADING : l = $.LOADED);
		this.color_ = e.color !== "undefined" ? _t(e.color) : null;
		this.iconImage_ = hs(o, a, {
			crossOrigin: this.crossOrigin_,
			referrerPolicy: this.referrerPolicy_
		}, l, this.color_);
		this.offset_ = e.offset !== "undefined" ? e.offset : [0, 0];
		this.offsetOrigin_ = e.offsetOrigin !== "undefined" ? e.offsetOrigin : "top-left";
		this.origin_ = null;
		this.size_ = e.size !== "undefined" ? e.size : null;
		this.initialOptions_;
		if (e.width !== "undefined" || e.height !== "undefined") {
			let h;
			let c;
			if (e.size) [h, c] = e.size;
			else {
				const u = this.getImage(1);
				if (u.width && u.height) {
					h = u.width;
					c = u.height;
				} else if (u instanceof HTMLImageElement) {
					this.initialOptions_ = e;
					const d = () => {
						this.unlistenImageChange(d);
						if (!this.initialOptions_) return;
						const f = this.iconImage_.getSize();
						this.setScale(cc(f[0], f[1], e.width, e.height));
					};
					this.listenImageChange(d);
					return;
				}
			}
			if (h !== "undefined") {
				this.setScale(cc(h, c, e.width, e.height));
			}
		}
	}
	clone() {
		let e, t, i;
		this.initialOptions_ ? (t = this.initialOptions_.width, i = this.initialOptions_.height) : (e = this.getScale(), e = Array.isArray(e) ? e.slice() : e);
		return new Nl({
			anchor: this.anchor_.slice(),
			anchorOrigin: this.anchorOrigin_,
			anchorXUnits: this.anchorXUnits_,
			anchorYUnits: this.anchorYUnits_,
			color: this.color_ && this.color_.slice ? this.color_.slice() : this.color_ || "undefined",
			crossOrigin: this.crossOrigin_,
			referrerPolicy: this.referrerPolicy_,
			offset: this.offset_.slice(),
			offsetOrigin: this.offsetOrigin_,
			opacity: this.getOpacity(),
			rotateWithView: this.getRotateWithView(),
			rotation: this.getRotation(),
			scale: e,
			width: t,
			height: i,
			size: this.size_ !== null ? this.size_.slice() : "undefined",
			src: this.getSrc(),
			displacement: this.getDisplacement().slice(),
			declutterMode: this.getDeclutterMode()
		});
	}
	getAnchor() {
		let e = this.normalizedAnchor_;
		if (!e) {
			e = this.anchor_;
			const s = this.getSize();
			if (this.anchorXUnits_ == "fraction" || this.anchorYUnits_ == "fraction") {
				if (!s) return null;
				e = this.anchor_.slice();
				if (this.anchorXUnits_ == "fraction") {
					e[0] *= s[0];
				}
				if (this.anchorYUnits_ == "fraction") {
					e[1] *= s[1];
				}
			}
			if (this.anchorOrigin_ != "top-left") {
				if (!s) return null;
				if (e === this.anchor_) {
					e = this.anchor_.slice();
				}
				if (this.anchorOrigin_ == "top-right" || this.anchorOrigin_ == "bottom-right") {
					e[0] = -e[0] + s[0];
				}
				if (this.anchorOrigin_ == "bottom-left" || this.anchorOrigin_ == "bottom-right") {
					e[1] = -e[1] + s[1];
				}
			}
			this.normalizedAnchor_ = e;
		}
		const t = this.getDisplacement(), i = this.getScaleArray();
		return [e[0] - t[0] / i[0], e[1] + t[1] / i[1]];
	}
	setAnchor(e) {
		this.anchor_ = e;
		this.normalizedAnchor_ = null;
	}
	getColor() {
		return this.color_;
	}
	setColor(e) {
		const t = e ? _t(e) : null;
		if (this.color_ === t || this.color_ && t && this.color_.length === t.length && this.color_.every((o, a) => o === t[a])) return;
		this.color_ = t;
		const i = this.getSrc(), s = i !== "undefined" ? null : this.getHitDetectionImage(), r = i !== "undefined" ? $.IDLE : this.iconImage_.getImageState();
		this.iconImage_ = hs(s, i, {
			crossOrigin: this.crossOrigin_,
			referrerPolicy: this.referrerPolicy_
		}, r, this.color_);
	}
	getImage(e) {
		return this.iconImage_.getImage(e);
	}
	getPixelRatio(e) {
		return this.iconImage_.getPixelRatio(e);
	}
	getImageSize() {
		return this.iconImage_.getSize();
	}
	getImageState() {
		return this.iconImage_.getImageState();
	}
	getHitDetectionImage() {
		return this.iconImage_.getHitDetectionImage();
	}
	getOrigin() {
		if (this.origin_) return this.origin_;
		let e = this.offset_;
		if (this.offsetOrigin_ != "top-left") {
			const t = this.getSize();
			const i = this.iconImage_.getSize();
			if (!t || !i) return null;
			e = e.slice();
			if (this.offsetOrigin_ == "top-right" || this.offsetOrigin_ == "bottom-right") {
				e[0] = i[0] - t[0] - e[0];
			}
			if (this.offsetOrigin_ == "bottom-left" || this.offsetOrigin_ == "bottom-right") {
				e[1] = i[1] - t[1] - e[1];
			}
		}
		this.origin_ = e;
		return this.origin_;
	}
	getSrc() {
		return this.iconImage_.getSrc();
	}
	setSrc(e) {
		this.iconImage_ = hs(null, e, {
			crossOrigin: this.crossOrigin_,
			referrerPolicy: this.referrerPolicy_
		}, $.IDLE, this.color_);
	}
	getSize() {
		return this.size_ ? this.size_ : this.iconImage_.getSize();
	}
	getWidth() {
		const e = this.getScaleArray();
		if (this.size_) return this.size_[0] * e[0];
		if (this.iconImage_.getImageState() == $.LOADED) return this.iconImage_.getSize()[0] * e[0];
	}
	getHeight() {
		const e = this.getScaleArray();
		if (this.size_) return this.size_[1] * e[1];
		if (this.iconImage_.getImageState() == $.LOADED) return this.iconImage_.getSize()[1] * e[1];
	}
	setScale(e) {
		delete this.initialOptions_;
		super.setScale(e);
	}
	listenImageChange(e) {
		this.iconImage_.addEventListener(U.CHANGE, e);
	}
	load() {
		this.iconImage_.load();
	}
	unlistenImageChange(e) {
		this.iconImage_.removeEventListener(U.CHANGE, e);
	}
	ready() {
		return this.iconImage_.ready();
	}
}
const et = .5;
function ed(n, e, t, i, s, r, o, a, l) {
	const h = l ? Dn(s) : s, c = n[0] * et, u = n[1] * et, d = Re(c, u);
	d.imageSmoothingEnabled = false;
	const f = d.canvas, g = new U_(d, et, s, null, o, a, l ? ao(Sg, l) : null), m = t.length, _ = Math.floor(16777215 / t.length), p = {};
	for (let E = 1; E <= t.length; ++E) {
		const x = t[E - 1];
		const T = t[E - 1].getStyleFunction() || i;
		if (!T) continue;
		let v = T(t[E - 1], r);
		if (!v) continue;
		if (!Array.isArray(v)) {
			v = [v];
		}
		const S = (E * _).toString(16).padStart(7, "#00000");
		for (let R = 0, I = v.length; R < I; ++R) {
			const N = v[R];
			const L = v[R].getGeometryFunction()(x);
			if (!L || !me(h, L.getExtent())) continue;
			const A = v[R].clone();
			const W = A.getFill();
			if (W) {
				W.setColor(S);
			}
			const w = A.getStroke();
			if (w) {
				w.setColor(S);
				w.setLineDash(null);
			}
			A.setText("undefined");
			const b = v[R].getImage();
			if (b) {
				const q = b.getImageSize();
				if (!q) continue;
				const ie = Re(q[0], q[1], "undefined", { alpha: false });
				const re = ie.canvas;
				ie.fillStyle = S;
				ie.fillRect(0, 0, ie.canvas.width, ie.canvas.height);
				A.setImage(new Nl({
					img: ie.canvas,
					anchor: b.getAnchor(),
					anchorXUnits: "pixels",
					anchorYUnits: "pixels",
					offset: b.getOrigin(),
					opacity: 1,
					size: b.getSize(),
					scale: b.getScale(),
					rotation: b.getRotation(),
					rotateWithView: b.getRotateWithView()
				}));
			}
			const D = A.getZIndex() || 0;
			let k = p[D];
			if (!k) {
				k = {};
				p[D] = k;
				k.Polygon = [];
				k.Circle = [];
				k.LineString = [];
				k.Point = [];
			}
			const B = L.getType();
			if (B === "GeometryCollection") {
				const q = L.getGeometriesArrayRecursive();
				for (let ie = 0, re = q.length; ie < re; ++ie) {
					const ce = q[ie];
					k[q[ie].getType().replace("Multi", "")].push(q[ie], A);
				}
			} else k[B.replace("Multi", "")].push(L, A);
		}
	}
	const y = Object.keys(p).map(Number).sort(Lt);
	for (let E = 0, x = y.length; E < x; ++E) {
		const T = p[y[E]];
		for (const v in p[y[E]]) {
			const P = T[v];
			for (let S = 0, R = T[v].length; S < R; S += 2) {
				g.setStyle(P[S + 1]);
				for (let I = 0, N = e.length; I < N; ++I) {
					g.setTransform(e[I]);
					g.drawGeometry(P[S]);
				}
			}
		}
	}
	return d.getImageData(0, 0, d.canvas.width, d.canvas.height);
}
function td(n, e, t) {
	const i = [];
	if (t) {
		const s = Math.floor(Math.round(n[0]) * et);
		const r = Math.floor(Math.round(n[1]) * et);
		const o = (fe(s, 0, t.width - 1) + fe(r, 0, t.height - 1) * t.width) * 4;
		const a = t.data[o];
		const l = t.data[o + 1];
		const c = t.data[o + 2] + 256 * (t.data[o + 1] + 256 * t.data[o]);
		const u = Math.floor(16777215 / e.length);
		if (c && c % u === 0) {
			i.push(e[c / u - 1]);
		}
	}
	return i;
}
class Sm extends wf {
	constructor(e, t, i, s) {
		super(e);
		this.inversePixelTransform = t;
		this.frameState = i;
		this.context = s;
	}
}
const vm = 5;
class wm extends Ls {
	constructor(e) {
		super();
		this.ready = true;
		this.boundHandleImageChange_ = this.handleImageChange_.bind(this);
		this.layer_ = e;
		this.staleKeys_ = new Array();
		this.maxStaleKeys = vm;
	}
	getStaleKeys() {
		return this.staleKeys_;
	}
	prependStaleKey(e) {
		this.staleKeys_.unshift(e);
		if (this.staleKeys_.length > this.maxStaleKeys) {
			this.staleKeys_.length = this.maxStaleKeys;
		}
	}
	getFeatures(e) {
		return z();
	}
	getData(e) {
		return null;
	}
	prepareFrame(e) {
		return z();
	}
	renderFrame(e, t) {
		return z();
	}
	forEachFeatureAtCoordinate(e, t, i, s, r) {}
	getLayer() {
		return this.layer_;
	}
	handleFontsChanged() {}
	handleImageChange_(e) {
		const t = e.target;
		if (e.target.getState() === $.LOADED || e.target.getState() === $.ERROR) {
			this.renderIfReadyAndVisible();
		}
	}
	loadImage(e) {
		let t = e.getState();
		if (t != $.LOADED && t != $.ERROR) {
			e.addEventListener(U.CHANGE, this.boundHandleImageChange_);
		}
		if (t == $.IDLE) {
			e.load();
			t = e.getState();
		}
		return t == $.LOADED;
	}
	renderIfReadyAndVisible() {
		const e = this.getLayer();
		if (e && e.getVisible() && e.getSourceState() === "ready") {
			e.changed();
		}
	}
	renderDeferred(e) {}
	disposeInternal() {
		delete this.layer_;
		super.disposeInternal();
	}
}
const uc = [];
let dn = null;
function Pm() {
	dn = Re(1, 1, "undefined", { willReadFrequently: true });
}
class Im extends wm {
	constructor(e) {
		super(e);
		this.container = null;
		this.renderedResolution;
		this.tempTransform = Se();
		this.pixelTransform = Se();
		this.inversePixelTransform = Se();
		this.context = null;
		this.deferredContext_ = null;
		this.containerReused = false;
		this.frameState = null;
	}
	getImageData(e, t, i) {
		Pm();
		dn.clearRect(0, 0, 1, 1);
		let s;
		try {
			dn.drawImage(e, t, i, 1, 1, 0, 0, 1, 1);
			s = dn.getImageData(0, 0, 1, 1).data;
		} catch (r) {
			dn = null;
			return null;
		}
		return s;
	}
	getBackground(e) {
		let i = this.getLayer().getBackground();
		if (typeof i == "function") {
			i = i(e.viewState.resolution);
		}
		return i || "undefined";
	}
	useContainer(e, t, i) {
		const s = this.getLayer().getClassName();
		let r, o;
		if (e && e.className === s && (!i || e && e.style.backgroundColor && kt(_t(e.style.backgroundColor), _t(i)))) {
			const a = e.firstElementChild;
			if (pi(e.firstElementChild)) {
				o = e.firstElementChild.getContext("2d");
			}
		}
		o && Ug(o.canvas.style.transform, t) ? (this.container = e, this.context = o, this.containerReused = true) : this.containerReused ? (this.container = null, this.context = null, this.containerReused = false) : this.container && (this.container.style.backgroundColor = null);
		if (!this.container) {
			r = ht ? Ou() : document.createElement("div");
			r.className = s;
			let a = r.style;
			a.position = "absolute";
			a.width = "100%";
			a.height = "100%";
			o = Re();
			const l = o.canvas;
			r.appendChild(o.canvas);
			a = o.canvas.style;
			a.position = "absolute";
			a.left = "0";
			a.transformOrigin = "top left";
			this.container = r;
			this.context = o;
		}
		if (!this.containerReused && i && !this.container.style.backgroundColor) {
			this.container.style.backgroundColor = i;
		}
	}
	clipUnrotated(e, t, i) {
		const s = ri(i), r = Ms(i), o = As(i), a = Sn(i);
		xe(t.coordinateToPixelTransform, s);
		xe(t.coordinateToPixelTransform, r);
		xe(t.coordinateToPixelTransform, o);
		xe(t.coordinateToPixelTransform, a);
		const l = this.inversePixelTransform;
		xe(this.inversePixelTransform, s);
		xe(this.inversePixelTransform, r);
		xe(this.inversePixelTransform, o);
		xe(this.inversePixelTransform, a);
		e.save();
		e.beginPath();
		e.moveTo(Math.round(s[0]), Math.round(s[1]));
		e.lineTo(Math.round(r[0]), Math.round(r[1]));
		e.lineTo(Math.round(o[0]), Math.round(o[1]));
		e.lineTo(Math.round(a[0]), Math.round(a[1]));
		e.clip();
	}
	prepareContainer(e, t) {
		const i = e.extent, s = e.viewState.resolution, r = e.viewState.rotation, o = e.pixelRatio, a = Math.round(J(e.extent) / e.viewState.resolution * e.pixelRatio), l = Math.round(Ce(e.extent) / e.viewState.resolution * e.pixelRatio);
		gt(this.pixelTransform, e.size[0] / 2, e.size[1] / 2, 1 / e.pixelRatio, 1 / e.pixelRatio, e.viewState.rotation, -a / 2, -l / 2);
		ps(this.inversePixelTransform, this.pixelTransform);
		const h = $g(this.pixelTransform);
		this.useContainer(t, h, this.getBackground(e));
		if (!this.containerReused) {
			const c = this.context.canvas;
			this.context.canvas.width != a || this.context.canvas.height != l ? (this.context.canvas.width = a, this.context.canvas.height = l) : this.context.clearRect(0, 0, a, l);
			if (h !== this.context.canvas.style.transform) {
				this.context.canvas.style.transform = h;
			}
		}
	}
	dispatchRenderEvent_(e, t, i) {
		const s = this.getLayer();
		if (s.hasListener(e)) {
			const r = new Sm(e, this.inversePixelTransform, i, t);
			s.dispatchEvent(r);
		}
	}
	preRender(e, t) {
		this.frameState = t;
		if (!t.declutter) {
			this.dispatchRenderEvent_(Me.PRERENDER, e, t);
		}
	}
	postRender(e, t) {
		if (!t.declutter) {
			this.dispatchRenderEvent_(Me.POSTRENDER, e, t);
		}
	}
	renderDeferredInternal(e) {}
	getRenderContext(e) {
		if (e.declutter && !this.deferredContext_) {
			this.deferredContext_ = new _m();
		}
		return e.declutter ? this.deferredContext_.getContext() : this.context;
	}
	renderDeferred(e) {
		if (e.declutter) {
			this.dispatchRenderEvent_(Me.PRERENDER, this.context, e);
			if (e.declutter && this.deferredContext_) {
				this.deferredContext_.draw(this.context);
				this.deferredContext_.clear();
			}
			this.renderDeferredInternal(e);
			this.dispatchRenderEvent_(Me.POSTRENDER, this.context, e);
		}
	}
	getRenderTransform(e, t, i, s, r, o, a) {
		const l = r / 2, h = o / 2, c = s / t, u = -c, d = -e[0] + a, f = -e[1];
		return gt(this.tempTransform, l, h, c, u, -i, d, f);
	}
	disposeInternal() {
		delete this.frameState;
		super.disposeInternal();
	}
}
class Fm extends Im {
	constructor(e) {
		super(e);
		this.boundHandleStyleImageChange_ = this.handleStyleImageChange_.bind(this);
		this.animatingOrInteracting_;
		this.hitDetectionImageData_ = null;
		this.clipExtent_ = null;
		this.extendX_ = false;
		this.renderedFeatures_ = null;
		this.renderedRevision_ = -1;
		this.renderedResolution_ = NaN;
		this.renderedExtent_ = je();
		this.wrappedRenderedExtent_ = je();
		this.renderedRotation_;
		this.renderedCenter_ = null;
		this.renderedProjection_ = null;
		this.renderedPixelRatio_ = 1;
		this.renderedRenderOrder_ = null;
		this.renderedFrameDeclutter_;
		this.replayGroup_ = null;
		this.replayGroupChanged = true;
		this.clipping = true;
		this.targetContext_ = null;
		this.opacity_ = 1;
	}
	renderWorlds(e, t, i) {
		const s = t.extent, r = t.viewState, o = t.viewState.center, a = t.viewState.resolution, l = t.viewState.projection, h = t.viewState.rotation, c = t.viewState.projection.getExtent(), u = this.getLayer().getSource(), d = this.getLayer().getDeclutter(), f = t.pixelRatio, g = t.viewHints, m = !(t.viewHints[de.ANIMATING] || t.viewHints[de.INTERACTING]), _ = this.context, p = Math.round(J(t.extent) / t.viewState.resolution * t.pixelRatio), y = Math.round(Ce(t.extent) / t.viewState.resolution * t.pixelRatio), E = u.getWrapX() && t.viewState.projection.canWrapX(), x = E ? J(c) : null, T = E ? Math.ceil((t.extent[2] - c[2]) / x) + (this.extendX_ ? 2 : 1) : 1;
		let v = E ? Math.floor((t.extent[0] - c[0]) / x) - (this.extendX_ ? 1 : 0) : 0;
		do {
			let P = this.getRenderTransform(o, a, 0, f, p, y, v * x);
			if (t.declutter) {
				P = P.slice(0);
			}
			e.execute(_, [_.canvas.width, _.canvas.height], P, h, m, i === "undefined" ? Ni : i ? Xr : Em, i ? d && t.declutter[d] : "undefined");
		} while (++v < T);
	}
	setDrawContext_() {
		if (this.opacity_ !== 1) {
			this.targetContext_ = this.context;
			this.context = Re(this.context.canvas.width, this.context.canvas.height, uc);
		}
	}
	resetDrawContext_() {
		if (this.opacity_ !== 1 && this.targetContext_) {
			const e = this.targetContext_.globalAlpha;
			this.targetContext_.globalAlpha = this.opacity_;
			this.targetContext_.drawImage(this.context.canvas, 0, 0);
			this.targetContext_.globalAlpha = this.targetContext_.globalAlpha;
			Ns(this.context);
			uc.push(this.context.canvas);
			this.context = this.targetContext_;
			this.targetContext_ = null;
		}
	}
	renderDeclutter(e) {
		if (!(!this.replayGroup_ || !this.getLayer().getDeclutter())) {
			this.renderWorlds(this.replayGroup_, e, true);
		}
	}
	renderDeferredInternal(e) {
		if (this.replayGroup_) {
			if (this.clipExtent_) {
				this.clipUnrotated(this.context, e, this.clipExtent_);
			}
			this.replayGroup_.renderDeferred();
			if (this.clipExtent_) {
				this.context.restore();
				this.clipExtent_ = null;
			}
			this.resetDrawContext_();
		}
	}
	renderFrame(e, t) {
		const i = e.layerStatesArray[e.layerIndex];
		this.opacity_ = e.layerStatesArray[e.layerIndex].opacity;
		const s = e.viewState;
		this.prepareContainer(e, t);
		const r = this.context, o = this.replayGroup_;
		let a = this.replayGroup_ && !this.replayGroup_.isEmpty();
		if (!a && !(this.getLayer().hasListener(Me.PRERENDER) || this.getLayer().hasListener(Me.POSTRENDER))) return this.container;
		this.setDrawContext_();
		this.preRender(this.context, e);
		e.viewState.projection;
		this.clipExtent_ = null;
		let l = false;
		if (a && e.layerStatesArray[e.layerIndex].extent && this.clipping) {
			const h = lt(i.extent);
			a = me(h, e.extent);
			if (a && !at(h, e.extent)) {
				e.declutter ? this.clipExtent_ = h : (this.clipUnrotated(r, e, h), l = true);
			}
		}
		if (a) {
			this.renderWorlds(this.replayGroup_, e, this.getLayer().getDeclutter() ? false : "undefined");
		}
		this.postRender(this.context, e);
		if (this.renderedRotation_ !== e.viewState.rotation) {
			this.renderedRotation_ = e.viewState.rotation;
			this.hitDetectionImageData_ = null;
		}
		if (!e.declutter) {
			this.resetDrawContext_();
		}
		return this.container;
	}
	getFeatures(e) {
		return new Promise((t) => {
			if (this.frameState && !this.hitDetectionImageData_ && !this.animatingOrInteracting_) {
				const i = this.frameState.size.slice();
				const s = this.renderedCenter_;
				const r = this.renderedResolution_;
				const o = this.renderedRotation_;
				const a = this.renderedProjection_;
				const l = this.wrappedRenderedExtent_;
				const h = this.getLayer();
				const c = [];
				const u = i[0] * et;
				const d = i[1] * et;
				c.push(this.getRenderTransform(this.renderedCenter_, this.renderedResolution_, this.renderedRotation_, et, u, d, 0).slice());
				const f = h.getSource();
				const g = this.renderedProjection_.getExtent();
				if (f.getWrapX() && this.renderedProjection_.canWrapX() && !at(g, this.wrappedRenderedExtent_)) {
					let m = l[0];
					const _ = J(g);
					let p = 0;
					let y;
					for (; m < g[0];) {
						--p;
						y = _ * p;
						c.push(this.getRenderTransform(s, r, o, et, u, d, y).slice());
						m += _;
					}
					for (p = 0, m = l[2]; m > g[2];) {
						++p;
						y = _ * p;
						c.push(this.getRenderTransform(s, r, o, et, u, d, y).slice());
						m -= _;
					}
				}
				this.hitDetectionImageData_ = ed(i, c, this.renderedFeatures_, h.getStyleFunction(), this.wrappedRenderedExtent_, this.renderedResolution_, this.renderedRotation_, Aa(this.renderedResolution_, this.renderedPixelRatio_), null);
			}
			t(td(e, this.renderedFeatures_, this.hitDetectionImageData_));
		});
	}
	forEachFeatureAtCoordinate(e, t, i, s, r) {
		var d, f;
		if (!this.replayGroup_) return;
		const o = t.viewState.resolution, a = t.viewState.rotation, l = this.getLayer(), h = {}, c = function(g, m, _) {
			const p = O(g), y = h[p];
			if (h[p]) {
				if (y !== true && _ < y.distanceSq) {
					y.geometry = m;
					y.distanceSq = _;
				}
			} else {
				r.push(h[p] = {
					feature: g,
					layer: l,
					geometry: m,
					distanceSq: _,
					callback: s
				});
			}
		}, u = this.getLayer().getDeclutter();
		return this.replayGroup_.forEachFeatureAtCoordinate(e, t.viewState.resolution, t.viewState.rotation, i, c, u ? (f = (d = t.declutter) == null ? "undefined" : d[u]) == null ? "undefined" : f.all().map((g) => g.value) : null);
	}
	handleFontsChanged() {
		const e = this.getLayer();
		if (e.getVisible() && this.replayGroup_) {
			e.changed();
		}
	}
	handleStyleImageChange_(e) {
		this.renderIfReadyAndVisible();
	}
	prepareFrame(e) {
		const t = this.getLayer(), i = t.getSource();
		if (!i) return false;
		const s = e.viewHints[de.ANIMATING], r = e.viewHints[de.INTERACTING], o = t.getUpdateWhileAnimating(), a = t.getUpdateWhileInteracting();
		if (this.ready && !o && e.viewHints[de.ANIMATING] || !a && e.viewHints[de.INTERACTING]) return this.animatingOrInteracting_ = true, true;
		this.animatingOrInteracting_ = false;
		const l = e.extent, h = e.viewState, c = e.viewState.projection, u = e.viewState.resolution, d = e.pixelRatio, f = t.getRevision(), g = t.getRenderBuffer();
		let m = t.getRenderOrder();
		if (m === "undefined") {
			m = z_;
		}
		const _ = e.viewState.center.slice(), p = tt(e.extent, g * e.viewState.resolution), y = p.slice(), E = [p.slice()], x = e.viewState.projection.getExtent(), T = i.getWrapX() && e.viewState.projection.canWrapX();
		this.extendX_ = false;
		if (T) {
			const w = i.getExtent();
			if (w && !Pi(w)) {
				this.extendX_ = w[0] < x[0] || w[2] > x[2];
			}
		}
		if (T && (!at(x, e.extent) || this.extendX_)) {
			const w = J(x);
			const b = Math.max(J(p) / 2, w);
			let D = x[0];
			let k = x[2];
			if (this.extendX_) {
				D -= w;
				k += w;
			}
			p[0] = D - b;
			p[2] = k + b;
			Ja(_, c);
			const B = Qc(E[0], c);
			B[0] < x[0] && B[2] < x[2] ? E.push([
				B[0] + w,
				B[1],
				B[2] + w,
				B[3]
			]) : B[0] > x[0] && B[2] > x[2] && E.push([
				B[0] - w,
				B[1],
				B[2] - w,
				B[3]
			]);
		}
		if (this.ready && this.renderedResolution_ == e.viewState.resolution && this.renderedPixelRatio_ === e.pixelRatio && this.renderedRevision_ == f && this.renderedRenderOrder_ == m && this.renderedFrameDeclutter_ === !!e.declutter && at(this.wrappedRenderedExtent_, p)) return kt(this.renderedExtent_, y) || (this.hitDetectionImageData_ = null, this.renderedExtent_ = y), this.renderedCenter_ = _, this.replayGroupChanged = false, true;
		this.replayGroup_ = null;
		const v = new fm(Xu(e.viewState.resolution, e.pixelRatio), p, e.viewState.resolution, e.pixelRatio);
		let P;
		for (let w = 0, b = E.length; w < b; ++w) i.loadFeatures(E[w], e.viewState.resolution, e.viewState.projection);
		const S = Aa(e.viewState.resolution, e.pixelRatio);
		let R = true;
		const I = (w, b) => {
			let D;
			const k = w.getStyleFunction() || t.getStyleFunction();
			if (k) {
				D = k(w, u);
			}
			if (D) {
				const B = this.renderFeature(w, S, D, v, P, this.getLayer().getDeclutter(), b);
				R = R && !B;
			}
		}, N = Dn(p), L = i.getFeaturesInExtent(N);
		if (m) {
			L.sort(m);
		}
		for (let w = 0, b = L.length; w < b; ++w) I(L[w], w);
		this.renderedFeatures_ = L;
		this.ready = R;
		const A = v.finish(), W = new Cm(p, e.viewState.resolution, e.pixelRatio, i.getOverlaps(), A, t.getRenderBuffer(), !!e.declutter);
		this.renderedResolution_ = e.viewState.resolution;
		this.renderedRevision_ = f;
		this.renderedRenderOrder_ = m;
		this.renderedFrameDeclutter_ = !!e.declutter;
		this.renderedExtent_ = y;
		this.wrappedRenderedExtent_ = p;
		this.renderedCenter_ = _;
		this.renderedProjection_ = e.viewState.projection;
		this.renderedPixelRatio_ = e.pixelRatio;
		this.replayGroup_ = W;
		this.hitDetectionImageData_ = null;
		this.replayGroupChanged = true;
		return true;
	}
	renderFeature(e, t, i, s, r, o, a) {
		if (!i) return false;
		let l = false;
		if (Array.isArray(i)) for (let h = 0, c = i.length; h < c; ++h) l = jr(s, e, i[h], t, this.boundHandleStyleImageChange_, r, o, a) || l;
		else l = jr(s, e, i, t, this.boundHandleStyleImageChange_, r, o, a);
		return l;
	}
}
let Zi = 0;
const we = 1 << Zi++;
const G = 1 << Zi++;
const Ie = 1 << Zi++;
const pe = 1 << Zi++;
const He = 1 << Zi++;
const ut = 1 << Zi++;
const nr = Math.pow(2, Zi) - 1;
const Gl = {
	[we]: "boolean",
	[G]: "number",
	[Ie]: "string",
	[pe]: "color",
	[He]: "number[]",
	[ut]: "size"
};
const Am = Object.keys(Gl).map(Number).sort(Lt);
function Mm(n) {
	return n in Gl;
}
function pn(n) {
	const e = [];
	for (const t of Am) ns(n, t) && e.push(Gl[t]);
	return e.length === 0 ? "untyped" : e.length < 3 ? e.join(" or ") : e.slice(0, -1).join(", ") + ", or " + e[e.length - 1];
}
function ns(n, e) {
	return (n & e) === e;
}
function Pt(n, e) {
	return n === e;
}
class Pe {
	constructor(e, t) {
		if (!Mm(e)) throw new Error("literal expressions must have a specific type, got ".concat(pn(e)));
		this.type = e;
		this.value = t;
	}
}
class nd {
	constructor(e, t, ...i) {
		this.type = e;
		this.operator = t;
		this.args = i;
	}
}
function Bl() {
	return {
		variables: new Set(),
		properties: new Set(),
		featureId: false,
		geometryType: false,
		mapState: false
	};
}
function be(n, e, t) {
	switch (typeof n) {
		case "boolean": {
			if (e === Ie) return new Pe(Ie, n ? "true" : "false");
			if (!ns(e, we)) throw new Error("got a boolean, but expected ".concat(pn(e)));
			return new Pe(we, n);
		}
		case "number": {
			if (e === ut) return new Pe(ut, Ne(n));
			if (e === we) return new Pe(we, !!n);
			if (e === Ie) return new Pe(Ie, n.toString());
			if (!ns(e, G)) throw new Error("got a number, but expected ".concat(pn(e)));
			return new Pe(G, n);
		}
		case "string": {
			if (e === pe) return new Pe(pe, Rl(n));
			if (e === we) return new Pe(we, !!n);
			if (!ns(e, Ie)) throw new Error("got a string, but expected ".concat(pn(e)));
			return new Pe(Ie, n);
		}
	}
	if (!Array.isArray(n)) throw new Error("expression must be an array or a primitive value");
	if (n.length === 0) throw new Error("empty expression");
	if (typeof n[0] == "string") return zm(n, e, t);
	for (const i of n) if (typeof i != "number") throw new Error("expected an array of numbers");
	if (e === ut) {
		if (n.length !== 2) throw new Error("expected an array of two values for a size, got ".concat(n.length));
		return new Pe(ut, n);
	}
	if (e === pe) {
		if (n.length === 3) return new Pe(pe, [...n, 1]);
		if (n.length === 4) return new Pe(pe, n);
		throw new Error("expected an array of 3 or 4 values for a color, got ".concat(n.length));
	}
	if (!ns(e, He)) throw new Error("got an array of numbers, but expected ".concat(pn(e)));
	return new Pe(He, n);
}
const C = {
	Get: "get",
	Var: "var",
	Concat: "concat",
	GeometryType: "geometry-type",
	LineMetric: "line-metric",
	Any: "any",
	All: "all",
	Not: "!",
	Resolution: "resolution",
	Zoom: "zoom",
	Time: "time",
	Equal: "==",
	NotEqual: "!=",
	GreaterThan: ">",
	GreaterThanOrEqualTo: ">=",
	LessThan: "<",
	LessThanOrEqualTo: "<=",
	Multiply: "*",
	Divide: "/",
	Add: "+",
	Subtract: "-",
	Clamp: "clamp",
	Mod: "%",
	Pow: "^",
	Abs: "abs",
	Floor: "floor",
	Ceil: "ceil",
	Round: "round",
	Sin: "sin",
	Cos: "cos",
	Atan: "atan",
	Sqrt: "sqrt",
	Match: "match",
	Between: "between",
	Interpolate: "interpolate",
	Coalesce: "coalesce",
	Case: "case",
	In: "in",
	Number: "number",
	String: "string",
	Array: "array",
	Color: "color",
	Id: "id",
	Band: "band",
	Palette: "palette",
	ToString: "to-string",
	Has: "has"
};
const bm = {
	[C.Get]: V(K(1, null), dc),
	[C.Var]: V(K(1, 1), Om),
	[C.Has]: V(K(1, null), dc),
	[C.Id]: V(Dm, en),
	[C.Concat]: V(K(2, null), oe(Ie)),
	[C.GeometryType]: V(Nm, en),
	[C.LineMetric]: V(en),
	[C.Resolution]: V(Yo, en),
	[C.Zoom]: V(Yo, en),
	[C.Time]: V(Yo, en),
	[C.Any]: V(K(2, null), oe(we)),
	[C.All]: V(K(2, null), oe(we)),
	[C.Not]: V(K(1, 1), oe(we)),
	[C.Equal]: V(K(2, 2), oe(nr)),
	[C.NotEqual]: V(K(2, 2), oe(nr)),
	[C.GreaterThan]: V(K(2, 2), oe(G)),
	[C.GreaterThanOrEqualTo]: V(K(2, 2), oe(G)),
	[C.LessThan]: V(K(2, 2), oe(G)),
	[C.LessThanOrEqualTo]: V(K(2, 2), oe(G)),
	[C.Multiply]: V(K(2, null), fc),
	[C.Coalesce]: V(K(2, null), fc),
	[C.Divide]: V(K(2, 2), oe(G)),
	[C.Add]: V(K(2, null), oe(G)),
	[C.Subtract]: V(K(2, 2), oe(G)),
	[C.Clamp]: V(K(3, 3), oe(G)),
	[C.Mod]: V(K(2, 2), oe(G)),
	[C.Pow]: V(K(2, 2), oe(G)),
	[C.Abs]: V(K(1, 1), oe(G)),
	[C.Floor]: V(K(1, 1), oe(G)),
	[C.Ceil]: V(K(1, 1), oe(G)),
	[C.Round]: V(K(1, 1), oe(G)),
	[C.Sin]: V(K(1, 1), oe(G)),
	[C.Cos]: V(K(1, 1), oe(G)),
	[C.Atan]: V(K(1, 2), oe(G)),
	[C.Sqrt]: V(K(1, 1), oe(G)),
	[C.Match]: V(K(4, null), gc, Gm),
	[C.Between]: V(K(3, 3), oe(G)),
	[C.Interpolate]: V(K(6, null), gc, Bm),
	[C.Case]: V(K(3, null), km, $m),
	[C.In]: V(K(2, 2), Um),
	[C.Number]: V(K(1, null), oe(nr)),
	[C.String]: V(K(1, null), oe(nr)),
	[C.Array]: V(K(1, null), oe(G)),
	[C.Color]: V(K(1, 4), oe(G)),
	[C.Band]: V(K(1, 3), oe(G)),
	[C.Palette]: V(K(2, 2), jm),
	[C.ToString]: V(K(1, 1), oe(we | G | Ie | pe))
};
function dc(n, e, t) {
	const i = n.length - 1, s = new Array(i);
	for (let r = 0; r < i; ++r) {
		const o = n[r + 1];
		switch (typeof n[r + 1]) {
			case "number": {
				s[r] = new Pe(G, o);
				break;
			}
			case "string": {
				s[r] = new Pe(Ie, o);
				break;
			}
			default: throw new Error("expected a string key or numeric array index for a get operation, got ".concat(n[r + 1]));
		}
		if (r === 0) {
			t.properties.add(String(n[r + 1]));
		}
	}
	return s;
}
function Om(n, e, t) {
	const i = n[1];
	if (typeof n[1] != "string") throw new Error("expected a string argument for var operation");
	t.variables.add(n[1]);
	return [new Pe(Ie, n[1])];
}
function Dm(n, e, t) {
	t.featureId = true;
}
function Nm(n, e, t) {
	t.geometryType = true;
}
function Yo(n, e, t) {
	t.mapState = true;
}
function en(n, e, t) {
	const i = n[0];
	if (n.length !== 1) throw new Error("expected no arguments for ".concat(n[0], " operation"));
	return [];
}
function K(n, e) {
	return function(t, i, s) {
		const r = t[0], o = t.length - 1;
		if (o < n || o > e) {
			const a = e === null ? "".concat(n, " or more") : "".concat(n, " to ").concat(e);
			throw new Error("expected ".concat(a, " arguments for ").concat(r, ", got ").concat(o));
		}
	};
}
function fc(n, e, t) {
	const i = n.length - 1, s = new Array(i);
	for (let r = 0; r < i; ++r) {
		const o = be(n[r + 1], e, t);
		s[r] = o;
	}
	return s;
}
function oe(n) {
	return function(e, t, i) {
		const s = e.length - 1, r = new Array(s);
		for (let o = 0; o < s; ++o) {
			const a = be(e[o + 1], n, i);
			r[o] = a;
		}
		return r;
	};
}
function km(n, e, t) {
	const i = n[0], s = n.length - 1;
	if (s % 2 === 0) throw new Error("expected an odd number of arguments for ".concat(n[0], ", got ").concat(s, " instead"));
}
function gc(n, e, t) {
	const i = n[0], s = n.length - 1;
	if (s % 2 === 1) throw new Error("expected an even number of arguments for operation ".concat(n[0], ", got ").concat(s, " instead"));
}
function Gm(n, e, t) {
	const i = n.length - 1, s = Ie | G | we, r = be(n[1], s, t), o = be(n[n.length - 1], e, t), a = new Array(i - 2);
	for (let l = 0; l < i - 2; l += 2) {
		try {
			const h = be(n[l + 2], r.type, t);
			a[l] = h;
		} catch (h) {
			throw new Error("failed to parse argument ".concat(l + 1, " of match expression: ").concat(h.message));
		}
		try {
			const h = be(n[l + 3], o.type, t);
			a[l + 1] = h;
		} catch (h) {
			throw new Error("failed to parse argument ".concat(l + 2, " of match expression: ").concat(h.message));
		}
	}
	return [
		r,
		...a,
		o
	];
}
function Bm(n, e, t) {
	const i = n[1];
	let s;
	switch (n[1][0]) {
		case "linear":
			s = 1;
			break;
		case "exponential":
			const l = n[1][1];
			if (typeof l != "number" || l <= 0) throw new Error("expected a number base for exponential interpolation" + ", got ".concat(JSON.stringify(l), " instead"));
			s = l;
			break;
		default: throw new Error("invalid interpolation type: ".concat(JSON.stringify(n[1])));
	}
	const r = new Pe(G, s);
	let o;
	try {
		o = be(n[2], G, t);
	} catch (l) {
		throw new Error("failed to parse argument 1 in interpolate expression: ".concat(l.message));
	}
	const a = new Array(n.length - 3);
	for (let l = 0; l < a.length; l += 2) {
		try {
			const h = be(n[l + 3], G, t);
			a[l] = h;
		} catch (h) {
			throw new Error("failed to parse argument ".concat(l + 2, " for interpolate expression: ").concat(h.message));
		}
		try {
			const h = be(n[l + 4], e, t);
			a[l + 1] = h;
		} catch (h) {
			throw new Error("failed to parse argument ".concat(l + 3, " for interpolate expression: ").concat(h.message));
		}
	}
	return [
		r,
		o,
		...a
	];
}
function $m(n, e, t) {
	const i = be(n[n.length - 1], e, t), s = new Array(n.length - 1);
	for (let r = 0; r < s.length - 1; r += 2) {
		try {
			const o = be(n[r + 1], we, t);
			s[r] = o;
		} catch (o) {
			throw new Error("failed to parse argument ".concat(r, " of case expression: ").concat(o.message));
		}
		try {
			const o = be(n[r + 2], i.type, t);
			s[r + 1] = o;
		} catch (o) {
			throw new Error("failed to parse argument ".concat(r + 1, " of case expression: ").concat(o.message));
		}
	}
	s[s.length - 1] = i;
	return s;
}
function Um(n, e, t) {
	let i = n[2];
	if (!Array.isArray(i)) throw new Error("the second argument for the \"in\" operator must be an array");
	let s;
	if (typeof i[0] == "string") {
		if (i[0] !== "literal") throw new Error("for the \"in\" operator, a string array should be wrapped in a \"literal\" operator to disambiguate from expressions");
		if (!Array.isArray(i[1])) throw new Error("failed to parse \"in\" expression: the literal operator must be followed by an array");
		i = i[1];
		s = Ie;
	} else s = G;
	const r = new Array(i.length);
	for (let a = 0; a < r.length; a++) try {
		const l = be(i[a], s, t);
		r[a] = l;
	} catch (l) {
		throw new Error("failed to parse haystack item ".concat(a, " for \"in\" expression: ").concat(l.message));
	}
	return [be(n[1], s, t), ...r];
}
function jm(n, e, t) {
	let i;
	try {
		i = be(n[1], G, t);
	} catch (o) {
		throw new Error("failed to parse first argument in palette expression: ".concat(o.message));
	}
	const s = n[2];
	if (!Array.isArray(n[2])) throw new Error("the second argument of palette must be an array");
	const r = new Array(n[2].length);
	for (let o = 0; o < r.length; o++) {
		let a;
		try {
			a = be(s[o], pe, t);
		} catch (l) {
			throw new Error("failed to parse color at index ".concat(o, " in palette expression: ").concat(l.message));
		}
		if (!(a instanceof Pe)) throw new Error("the palette color at index ".concat(o, " must be a literal value"));
		r[o] = a;
	}
	return [i, ...r];
}
function V(...n) {
	return function(e, t, i) {
		const s = e[0];
		let r;
		for (let o = 0; o < n.length; o++) {
			const a = n[o](e, t, i);
			if (o == n.length - 1) {
				if (!a) throw new Error("expected last argument validator to return the parsed args");
				r = a;
			}
		}
		return new nd(t, e[0], ...r);
	};
}
function zm(n, e, t) {
	const i = n[0], s = bm[n[0]];
	if (!bm[n[0]]) throw new Error("unknown operator: ".concat(n[0]));
	return bm[n[0]](n, e, t);
}
function $l(n) {
	if (!n) return "";
	const e = n.getType();
	switch (e) {
		case "Point":
		case "LineString":
		case "Polygon": return e;
		case "MultiPoint":
		case "MultiLineString":
		case "MultiPolygon": return e.substring(5);
		case "Circle": return "Polygon";
		case "GeometryCollection": return $l(n.getGeometries()[0]);
		default: return "";
	}
}
function sd() {
	return {
		variables: {},
		properties: {},
		resolution: NaN,
		featureId: null,
		geometryType: ""
	};
}
function Nt(n, e, t) {
	const i = be(n, e, t);
	return Ct(i);
}
function Ct(n, e) {
	if (n instanceof Pe) {
		if (n.type === pe && typeof n.value == "string") {
			const i = Rl(n.value);
			return function() {
				return i;
			};
		}
		return function() {
			return n.value;
		};
	}
	const t = n.operator;
	switch (n.operator) {
		case C.Number:
		case C.String:
		case C.Coalesce: return Xm(n);
		case C.Get:
		case C.Var:
		case C.Has: return Wm(n);
		case C.Id: return (i) => i.featureId;
		case C.GeometryType: return (i) => i.geometryType;
		case C.Concat: {
			const i = n.args.map((s) => Ct(s));
			return (s) => "".concat(...i.map((r) => r(s).toString()));
		}
		case C.Resolution: return (i) => i.resolution;
		case C.Any:
		case C.All:
		case C.Between:
		case C.In:
		case C.Not: return Ym(n);
		case C.Equal:
		case C.NotEqual:
		case C.LessThan:
		case C.LessThanOrEqualTo:
		case C.GreaterThan:
		case C.GreaterThanOrEqualTo: return Vm(n);
		case C.Multiply:
		case C.Divide:
		case C.Add:
		case C.Subtract:
		case C.Clamp:
		case C.Mod:
		case C.Pow:
		case C.Abs:
		case C.Floor:
		case C.Ceil:
		case C.Round:
		case C.Sin:
		case C.Cos:
		case C.Atan:
		case C.Sqrt: return Zm(n);
		case C.Case: return Km(n);
		case C.Match: return Hm(n);
		case C.Interpolate: return qm(n);
		case C.ToString: return Jm(n);
		default: throw new Error("Unsupported operator ".concat(n.operator));
	}
}
function Xm(n, e) {
	const t = n.operator, i = n.args.length, s = new Array(n.args.length);
	for (let r = 0; r < n.args.length; ++r) s[r] = Ct(n.args[r]);
	switch (n.operator) {
		case C.Coalesce: return (r) => {
			for (let o = 0; o < i; ++o) {
				const a = s[o](r);
				if (typeof a < "u" && a !== null) return a;
			}
			throw new Error("Expected one of the values to be non-null");
		};
		case C.Number:
		case C.String: return (r) => {
			for (let o = 0; o < i; ++o) {
				const a = s[o](r);
				if (typeof a === t) return a;
			}
			throw new Error("Expected one of the values to be a ".concat(t));
		};
		default: throw new Error("Unsupported assertion operator ".concat(n.operator));
	}
}
function Wm(n, e) {
	const i = n.args[0].value;
	switch (n.operator) {
		case C.Get: return (s) => {
			const r = n.args;
			let o = s.properties[i];
			for (let a = 1, l = n.args.length; a < l; ++a) {
				const c = r[a].value;
				o = o[r[a].value];
			}
			return o;
		};
		case C.Var: return (s) => s.variables[i];
		case C.Has: return (s) => {
			const r = n.args;
			if (!(i in s.properties)) return false;
			let o = s.properties[i];
			for (let a = 1, l = n.args.length; a < l; ++a) {
				const c = r[a].value;
				if (!o || !Object.hasOwn(o, r[a].value)) return false;
				o = o[r[a].value];
			}
			return true;
		};
		default: throw new Error("Unsupported accessor operator ".concat(n.operator));
	}
}
function Vm(n, e) {
	const t = n.operator, i = Ct(n.args[0]), s = Ct(n.args[1]);
	switch (n.operator) {
		case C.Equal: return (r) => i(r) === s(r);
		case C.NotEqual: return (r) => i(r) !== s(r);
		case C.LessThan: return (r) => i(r) < s(r);
		case C.LessThanOrEqualTo: return (r) => i(r) <= s(r);
		case C.GreaterThan: return (r) => i(r) > s(r);
		case C.GreaterThanOrEqualTo: return (r) => i(r) >= s(r);
		default: throw new Error("Unsupported comparison operator ".concat(n.operator));
	}
}
function Ym(n, e) {
	const t = n.operator, i = n.args.length, s = new Array(n.args.length);
	for (let r = 0; r < n.args.length; ++r) s[r] = Ct(n.args[r]);
	switch (n.operator) {
		case C.Any: return (r) => {
			for (let o = 0; o < i; ++o) if (s[o](r)) return true;
			return false;
		};
		case C.All: return (r) => {
			for (let o = 0; o < i; ++o) if (!s[o](r)) return false;
			return true;
		};
		case C.Between: return (r) => {
			const o = s[0](r), a = s[1](r), l = s[2](r);
			return o >= a && o <= l;
		};
		case C.In: return (r) => {
			const o = s[0](r);
			for (let a = 1; a < i; ++a) if (o === s[a](r)) return true;
			return false;
		};
		case C.Not: return (r) => !s[0](r);
		default: throw new Error("Unsupported logical operator ".concat(n.operator));
	}
}
function Zm(n, e) {
	const t = n.operator, i = n.args.length, s = new Array(n.args.length);
	for (let r = 0; r < n.args.length; ++r) s[r] = Ct(n.args[r]);
	switch (n.operator) {
		case C.Multiply: return (r) => {
			let o = 1;
			for (let a = 0; a < i; ++a) o *= s[a](r);
			return o;
		};
		case C.Divide: return (r) => s[0](r) / s[1](r);
		case C.Add: return (r) => {
			let o = 0;
			for (let a = 0; a < i; ++a) o += s[a](r);
			return o;
		};
		case C.Subtract: return (r) => s[0](r) - s[1](r);
		case C.Clamp: return (r) => {
			const o = s[0](r), a = s[1](r);
			if (o < a) return a;
			const l = s[2](r);
			return o > l ? l : o;
		};
		case C.Mod: return (r) => s[0](r) % s[1](r);
		case C.Pow: return (r) => Math.pow(s[0](r), s[1](r));
		case C.Abs: return (r) => Math.abs(s[0](r));
		case C.Floor: return (r) => Math.floor(s[0](r));
		case C.Ceil: return (r) => Math.ceil(s[0](r));
		case C.Round: return (r) => Math.round(s[0](r));
		case C.Sin: return (r) => Math.sin(s[0](r));
		case C.Cos: return (r) => Math.cos(s[0](r));
		case C.Atan: return n.args.length === 2 ? (r) => Math.atan2(s[0](r), s[1](r)) : (r) => Math.atan(s[0](r));
		case C.Sqrt: return (r) => Math.sqrt(s[0](r));
		default: throw new Error("Unsupported numeric operator ".concat(n.operator));
	}
}
function Km(n, e) {
	const t = n.args.length, i = new Array(n.args.length);
	for (let s = 0; s < n.args.length; ++s) i[s] = Ct(n.args[s]);
	return (s) => {
		for (let r = 0; r < t - 1; r += 2) if (i[r](s)) return i[r + 1](s);
		return i[t - 1](s);
	};
}
function Hm(n, e) {
	const t = n.args.length, i = new Array(n.args.length);
	for (let s = 0; s < n.args.length; ++s) i[s] = Ct(n.args[s]);
	return (s) => {
		const r = i[0](s);
		for (let o = 1; o < t - 1; o += 2) if (r === i[o](s)) return i[o + 1](s);
		return i[t - 1](s);
	};
}
function qm(n, e) {
	const t = n.args.length, i = new Array(n.args.length);
	for (let s = 0; s < n.args.length; ++s) i[s] = Ct(n.args[s]);
	return (s) => {
		const r = i[0](s), o = i[1](s);
		let a, l;
		for (let h = 2; h < t; h += 2) {
			const c = i[h](s);
			let u = i[h + 1](s);
			const d = Array.isArray(u);
			if (d) {
				u = C_(u);
			}
			if (c >= o) return h === 2 ? u : d ? Qm(r, o, a, l, c, u) : ss(r, o, a, l, c, u);
			a = c;
			l = u;
		}
		return l;
	};
}
function Jm(n, e) {
	const t = n.operator, i = n.args.length, s = new Array(n.args.length);
	for (let r = 0; r < n.args.length; ++r) s[r] = Ct(n.args[r]);
	switch (n.operator) {
		case C.ToString: return (r) => {
			const o = s[0](r);
			return n.args[0].type === pe ? Sl(o) : o.toString();
		};
		default: throw new Error("Unsupported convert operator ".concat(n.operator));
	}
}
function ss(n, e, t, i, s, r) {
	const o = s - t;
	if (o === 0) return i;
	const a = e - t, l = n === 1 ? a / o : (Math.pow(n, a) - 1) / (Math.pow(n, o) - 1);
	return i + l * (r - i);
}
function Qm(n, e, t, i, s, r) {
	if (s - t === 0) return i;
	const a = Qh(i), l = Qh(r);
	let h = l[2] - a[2];
	h > 180 ? h -= 360 : h < -180 && (h += 360);
	const c = [
		ss(n, e, t, a[0], s, l[0]),
		ss(n, e, t, a[1], s, l[1]),
		a[2] + ss(n, e, t, 0, s, h),
		ss(n, e, t, i[3], s, r[3])
	];
	return R_(c);
}
function ep(n) {
	return true;
}
function tp(n) {
	const e = Bl(), t = ip(n, e), i = sd();
	return function(s, r) {
		i.properties = s.getPropertiesInternal();
		i.resolution = r;
		if (e.featureId) {
			const o = s.getId();
			o !== "undefined" ? i.featureId = o : i.featureId = null;
		}
		if (e.geometryType) {
			i.geometryType = $l(s.getGeometry());
		}
		return t(i);
	};
}
function _c(n) {
	const e = Bl(), t = n.length, i = new Array(n.length);
	for (let o = 0; o < n.length; ++o) i[o] = Ma(n[o], e);
	const s = sd(), r = new Array(n.length);
	return function(o, a) {
		s.properties = o.getPropertiesInternal();
		s.resolution = a;
		if (e.featureId) {
			const h = o.getId();
			h !== "undefined" ? s.featureId = h : s.featureId = null;
		}
		let l = 0;
		for (let h = 0; h < t; ++h) {
			const c = i[h](s);
			if (c) {
				r[l] = c;
				l += 1;
			}
		}
		r.length = l;
		return r;
	};
}
function ip(n, e) {
	const t = n.length, i = new Array(n.length);
	for (let s = 0; s < n.length; ++s) {
		const r = n[s];
		const o = "filter" in n[s] ? Nt(n[s].filter, we, e) : ep;
		let a;
		if (Array.isArray(n[s].style)) {
			const l = r.style.length;
			a = new Array(r.style.length);
			for (let h = 0; h < r.style.length; ++h) a[h] = Ma(r.style[h], e);
		} else a = [Ma(n[s].style, e)];
		i[s] = {
			filter: o,
			styles: a
		};
	}
	return function(s) {
		const r = [];
		let o = false;
		for (let a = 0; a < t; ++a) {
			const l = i[a].filter;
			if (i[a].filter(s) && !(n[a].else && o)) {
				o = true;
				for (const h of i[a].styles) {
					const c = h(s);
					if (c) {
						r.push(c);
					}
				}
			}
		}
		return r;
	};
}
function Ma(n, e) {
	const t = Ss(n, "", e), i = vs(n, "", e), s = np(n, e), r = sp(n, e), o = De(n, "z-index", e);
	if (!t && !i && !s && !r && !si(n)) throw new Error("No fill, stroke, point, or text symbolizer properties in style: " + JSON.stringify(n));
	const a = new yi();
	return function(l) {
		let h = true;
		if (t) {
			const c = t(l);
			if (c) {
				h = false;
			}
			a.setFill(c);
		}
		if (i) {
			const c = i(l);
			if (c) {
				h = false;
			}
			a.setStroke(c);
		}
		if (s) {
			const c = s(l);
			if (c) {
				h = false;
			}
			a.setText(c);
		}
		if (r) {
			const c = r(l);
			if (c) {
				h = false;
			}
			a.setImage(c);
		}
		if (o) {
			a.setZIndex(o(l));
		}
		return h ? null : a;
	};
}
function Ss(n, e, t) {
	let i;
	if (e + "fill-pattern-src" in n) i = lp(n, e + "fill-", t);
	else {
		if (n[e + "fill-color"] === "none") return (r) => null;
		i = go(n, e + "fill-color", t);
	}
	if (!i) return null;
	const s = new Pl();
	return function(r) {
		const o = i(r);
		return o === Cl ? null : (s.setColor(o), s);
	};
}
function vs(n, e, t) {
	const i = De(n, e + "stroke-width", t), s = go(n, e + "stroke-color", t);
	if (!i && !s) return null;
	const r = Kt(n, e + "stroke-line-cap", t), o = Kt(n, e + "stroke-line-join", t), a = rd(n, e + "stroke-line-dash", t), l = De(n, e + "stroke-line-dash-offset", t), h = De(n, e + "stroke-miter-limit", t), c = De(n, e + "stroke-offset", t), u = new Il();
	return function(d) {
		if (s) {
			const f = s(d);
			if (f === Cl) return null;
			u.setColor(f);
		}
		if (i) {
			u.setWidth(i(d));
		}
		if (r) {
			const f = r(d);
			if (f !== "butt" && f !== "round" && f !== "square") throw new Error("Expected butt, round, or square line cap");
			u.setLineCap(f);
		}
		if (o) {
			const f = o(d);
			if (f !== "bevel" && f !== "round" && f !== "miter") throw new Error("Expected bevel, round, or miter line join");
			u.setLineJoin(f);
		}
		if (a) {
			u.setLineDash(a(d));
		}
		if (l) {
			u.setLineDashOffset(l(d));
		}
		if (h) {
			u.setMiterLimit(h(d));
		}
		if (c) {
			u.setOffset(c(d));
		}
		return u;
	};
}
function np(n, e) {
	const t = "text-", i = Kt(n, "text-value", e);
	if (!i) return null;
	const s = Ss(n, "text-", e), r = Ss(n, "text-background-", e), o = vs(n, "text-", e), a = vs(n, "text-background-", e), l = Kt(n, "text-font", e), h = De(n, "text-max-angle", e), c = De(n, "text-offset-x", e), u = De(n, "text-offset-y", e), d = En(n, "text-overflow", e), f = Kt(n, "text-placement", e), g = De(n, "text-repeat", e), m = _o(n, "text-scale", e), _ = En(n, "text-rotate-with-view", e), p = De(n, "text-rotation", e), y = Kt(n, "text-align", e), E = Kt(n, "text-justify", e), x = Kt(n, "text-baseline", e), T = En(n, "text-keep-upright", e), v = rd(n, "text-padding", e), P = mo(n, "text-declutter-mode"), S = new Ol({ declutterMode: P });
	return function(R) {
		S.setText(i(R));
		if (s) {
			S.setFill(s(R));
		}
		if (r) {
			S.setBackgroundFill(r(R));
		}
		if (o) {
			S.setStroke(o(R));
		}
		if (a) {
			S.setBackgroundStroke(a(R));
		}
		if (l) {
			S.setFont(l(R));
		}
		if (h) {
			S.setMaxAngle(h(R));
		}
		if (c) {
			S.setOffsetX(c(R));
		}
		if (u) {
			S.setOffsetY(u(R));
		}
		if (d) {
			S.setOverflow(d(R));
		}
		if (f) {
			const I = f(R);
			if (I !== "point" && I !== "line") throw new Error("Expected point or line for text-placement");
			S.setPlacement(I);
		}
		if (g) {
			S.setRepeat(g(R));
		}
		if (m) {
			S.setScale(m(R));
		}
		if (_) {
			S.setRotateWithView(_(R));
		}
		if (p) {
			S.setRotation(p(R));
		}
		if (y) {
			const I = y(R);
			if (I !== "left" && I !== "center" && I !== "right" && I !== "end" && I !== "start") throw new Error("Expected left, right, center, start, or end for text-align");
			S.setTextAlign(I);
		}
		if (E) {
			const I = E(R);
			if (I !== "left" && I !== "right" && I !== "center") throw new Error("Expected left, right, or center for text-justify");
			S.setJustify(I);
		}
		if (x) {
			const I = x(R);
			if (I !== "bottom" && I !== "top" && I !== "middle" && I !== "alphabetic" && I !== "hanging") throw new Error("Expected bottom, top, middle, alphabetic, or hanging for text-baseline");
			S.setTextBaseline(I);
		}
		if (v) {
			S.setPadding(v(R));
		}
		if (T) {
			S.setKeepUpright(T(R));
		}
		return S;
	};
}
function sp(n, e) {
	return "icon-src" in n ? rp(n, e) : "shape-points" in n ? op(n, e) : "circle-radius" in n ? ap(n, e) : null;
}
function rp(n, e) {
	const t = "icon-", i = "icon-src", s = od(n[i], "icon-src"), r = Wr(n, "icon-anchor", e), o = _o(n, "icon-scale", e), a = De(n, "icon-opacity", e), l = Wr(n, "icon-displacement", e), h = De(n, "icon-rotation", e), c = En(n, "icon-rotate-with-view", e), u = pc(n, "icon-anchor-origin"), d = yc(n, "icon-anchor-x-units"), f = yc(n, "icon-anchor-y-units"), g = oi(n, "icon-color");
	let m, _ = null;
	if (g !== "undefined") {
		Array.isArray(g) && g.length > 0 && typeof g[0] == "string" ? _ = go(n, "icon-color", e) : m = ad(g, "icon-color");
	}
	const p = cp(n, "icon-cross-origin"), y = up(n, "icon-offset"), E = pc(n, "icon-offset-origin"), x = ba(n, "icon-width"), T = ba(n, "icon-height"), v = hp(n, "icon-size"), P = mo(n, "icon-declutter-mode"), S = {
		src: s,
		anchorOrigin: u,
		anchorXUnits: d,
		anchorYUnits: f,
		crossOrigin: p,
		offset: y,
		offsetOrigin: E,
		height: T,
		width: x,
		size: v,
		declutterMode: P
	};
	let R = null;
	return function(I) {
		const N = _ ? _(I) : m;
		R = new Nl(N !== "undefined" ? Object.assign({}, S, { color: N }) : Object.assign({}, S));
		if (a) {
			R.setOpacity(a(I));
		}
		if (l) {
			R.setDisplacement(l(I));
		}
		if (h) {
			R.setRotation(h(I));
		}
		if (c) {
			R.setRotateWithView(c(I));
		}
		if (o) {
			R.setScale(o(I));
		}
		if (r) {
			R.setAnchor(r(I));
		}
		return R;
	};
}
function op(n, e) {
	const t = "shape-", i = "shape-points", s = "shape-radius", r = Ul(n[i], "shape-points");
	if (!("shape-radius" in n)) throw new Error("Expected a number for shape-radius");
	const o = De(n, "shape-radius", e), a = typeof n[s] == "number" ? n[s] : 5, l = "shape-radius2", h = De(n, "shape-radius2", e), c = typeof n[l] == "number" ? n[l] : "undefined", u = Ss(n, "shape-", e), d = vs(n, "shape-", e), f = _o(n, "shape-scale", e), g = Wr(n, "shape-displacement", e), m = De(n, "shape-rotation", e), _ = En(n, "shape-rotate-with-view", e), p = ba(n, "shape-angle"), y = mo(n, "shape-declutter-mode"), E = new Ll({
		points: r,
		radius: a,
		radius2: c,
		angle: p,
		declutterMode: y
	});
	return function(x) {
		if (o) {
			E.setRadius(o(x));
		}
		if (h) {
			E.setRadius2(h(x));
		}
		if (u) {
			E.setFill(u(x));
		}
		if (d) {
			E.setStroke(d(x));
		}
		if (g) {
			E.setDisplacement(g(x));
		}
		if (m) {
			E.setRotation(m(x));
		}
		if (_) {
			E.setRotateWithView(_(x));
		}
		if (f) {
			E.setScale(f(x));
		}
		return E;
	};
}
function ap(n, e) {
	const t = "circle-", i = Ss(n, "circle-", e), s = vs(n, "circle-", e), r = De(n, "circle-radius", e), o = _o(n, "circle-scale", e), a = Wr(n, "circle-displacement", e), l = De(n, "circle-rotation", e), h = En(n, "circle-rotate-with-view", e), c = mo(n, "circle-declutter-mode"), u = new Al({
		radius: 5,
		declutterMode: c
	});
	return function(d) {
		if (r) {
			u.setRadius(r(d));
		}
		if (i) {
			u.setFill(i(d));
		}
		if (s) {
			u.setStroke(s(d));
		}
		if (a) {
			u.setDisplacement(a(d));
		}
		if (l) {
			u.setRotation(l(d));
		}
		if (h) {
			u.setRotateWithView(h(d));
		}
		if (o) {
			u.setScale(o(d));
		}
		return u;
	};
}
function oi(n, e) {
	if (!(e in n)) return;
	const t = n[e];
	return n[e] === "undefined" ? "undefined" : n[e];
}
function De(n, e, t) {
	const i = oi(n, e);
	if (i === "undefined") return;
	const s = Nt(i, G, t);
	return function(r) {
		return Ul(s(r), e);
	};
}
function Kt(n, e, t) {
	const i = oi(n, e);
	if (i === "undefined") return null;
	const s = Nt(i, Ie, t);
	return function(r) {
		return od(s(r), e);
	};
}
function lp(n, e, t) {
	const i = Kt(n, e + "pattern-src", t), s = mc(n, e + "pattern-offset", t), r = mc(n, e + "pattern-size", t), o = go(n, e + "color", t);
	return function(a) {
		return {
			src: i(a),
			offset: s && s(a),
			size: r && r(a),
			color: o && o(a)
		};
	};
}
function En(n, e, t) {
	const i = oi(n, e);
	if (i === "undefined") return null;
	const s = Nt(i, we, t);
	return function(r) {
		const o = s(r);
		if (typeof o != "boolean") throw new Error("Expected a boolean for ".concat(e));
		return o;
	};
}
function go(n, e, t) {
	const i = oi(n, e);
	if (i === "undefined") return null;
	const s = Nt(i, pe, t);
	return function(r) {
		return ad(s(r), e);
	};
}
function rd(n, e, t) {
	const i = oi(n, e);
	if (i === "undefined") return null;
	if (Array.isArray(i) && (i.length === 0 || typeof i[0] != "string")) {
		const r = i.map((o, a) => {
			if (typeof o == "number") return () => o;
			const l = Nt(o, G, t);
			return function(h) {
				return Ul(l(h), "".concat(e, "[").concat(a, "]"));
			};
		});
		return function(o) {
			const a = new Array(r.length);
			for (let l = 0; l < r.length; ++l) a[l] = r[l](o);
			return a;
		};
	}
	const s = Nt(i, He, t);
	return function(r) {
		return Bs(s(r), e);
	};
}
function Wr(n, e, t) {
	const i = oi(n, e);
	if (i === "undefined") return null;
	const s = Nt(i, He, t);
	return function(r) {
		const o = Bs(s(r), e);
		if (o.length !== 2) throw new Error("Expected two numbers for ".concat(e));
		return o;
	};
}
function mc(n, e, t) {
	const i = oi(n, e);
	if (i === "undefined") return null;
	const s = Nt(i, He, t);
	return function(r) {
		return ld(s(r), e);
	};
}
function _o(n, e, t) {
	const i = oi(n, e);
	if (i === "undefined") return null;
	const s = Nt(i, He | G, t);
	return function(r) {
		return dp(s(r), e);
	};
}
function ba(n, e) {
	const t = n[e];
	if (n[e] !== "undefined") {
		if (typeof t != "number") throw new Error("Expected a number for ".concat(e));
		return t;
	}
}
function hp(n, e) {
	const t = n[e];
	if (n[e] !== "undefined") {
		if (typeof t == "number") return Ne(t);
		if (!Array.isArray(t)) throw new Error("Expected a number or size array for ".concat(e));
		if (t.length !== 2 || typeof t[0] != "number" || typeof t[1] != "number") throw new Error("Expected a number or size array for ".concat(e));
		return t;
	}
}
function cp(n, e) {
	const t = n[e];
	if (n[e] !== "undefined") {
		if (typeof t != "string") throw new Error("Expected a string for ".concat(e));
		return t;
	}
}
function pc(n, e) {
	const t = n[e];
	if (n[e] !== "undefined") {
		if (t !== "bottom-left" && t !== "bottom-right" && t !== "top-left" && t !== "top-right") throw new Error("Expected bottom-left, bottom-right, top-left, or top-right for ".concat(e));
		return t;
	}
}
function yc(n, e) {
	const t = n[e];
	if (n[e] !== "undefined") {
		if (t !== "pixels" && t !== "fraction") throw new Error("Expected pixels or fraction for ".concat(e));
		return t;
	}
}
function up(n, e) {
	const t = n[e];
	if (n[e] !== "undefined") return Bs(n[e], e);
}
function mo(n, e) {
	const t = n[e];
	if (n[e] !== "undefined") {
		if (typeof t != "string") throw new Error("Expected a string for ".concat(e));
		if (t !== "declutter" && t !== "obstacle" && t !== "none") throw new Error("Expected declutter, obstacle, or none for ".concat(e));
		return t;
	}
}
function Bs(n, e) {
	if (!Array.isArray(n)) throw new Error("Expected an array for ".concat(e));
	const t = n.length;
	for (let i = 0; i < n.length; ++i) if (typeof n[i] != "number") throw new Error("Expected an array of numbers for ".concat(e));
	return n;
}
function od(n, e) {
	if (typeof n != "string") throw new Error("Expected a string for ".concat(e));
	return n;
}
function Ul(n, e) {
	if (typeof n != "number") throw new Error("Expected a number for ".concat(e));
	return n;
}
function ad(n, e) {
	if (typeof n == "string") return n;
	const t = Bs(n, e), i = t.length;
	if (t.length < 3 || t.length > 4) throw new Error("Expected a color with 3 or 4 values for ".concat(e));
	return t;
}
function ld(n, e) {
	const t = Bs(n, e);
	if (t.length !== 2) throw new Error("Expected an array of two numbers for ".concat(e));
	return t;
}
function dp(n, e) {
	return typeof n == "number" ? n : ld(n, e);
}
const pt = {
	CENTER: "center",
	RESOLUTION: "resolution",
	ROTATION: "rotation"
};
function xc(n, e, t) {
	return function(i, s, r, o, a) {
		if (!i) return;
		if (!s && !e) return i;
		const l = e ? 0 : r[0] * s, h = e ? 0 : r[1] * s, c = a ? a[0] : 0, u = a ? a[1] : 0;
		let d = n[0] + l / 2 + c, f = n[2] - l / 2 + c, g = n[1] + h / 2 + u, m = n[3] - h / 2 + u;
		if (d > f) {
			d = (f + d) / 2;
			f = d;
		}
		if (g > m) {
			g = (m + g) / 2;
			m = g;
		}
		let _ = fe(i[0], d, f), p = fe(i[1], g, m);
		if (o && t && s) {
			const y = 30 * s;
			_ += -y * Math.log(1 + Math.max(0, d - i[0]) / y) + y * Math.log(1 + Math.max(0, i[0] - f) / y);
			p += -y * Math.log(1 + Math.max(0, g - i[1]) / y) + y * Math.log(1 + Math.max(0, i[1] - m) / y);
		}
		return [_, p];
	};
}
function fp(n) {
	return n;
}
function hd(n) {
	return Math.pow(n, 3);
}
function $n(n) {
	return 1 - hd(1 - n);
}
function gp(n) {
	return 3 * n * n - 2 * n * n * n;
}
function _p(n) {
	return n;
}
function jl(n, e, t, i) {
	const s = J(e) / t[0], r = Ce(e) / t[1];
	return i ? Math.min(n, Math.max(s, r)) : Math.min(n, Math.min(s, r));
}
function zl(n, e, t) {
	let i = Math.min(n, e);
	const s = 50;
	i *= Math.log(1 + 50 * Math.max(0, n / e - 1)) / 50 + 1;
	if (t) {
		i = Math.max(i, t);
		i /= Math.log(1 + 50 * Math.max(0, t / n - 1)) / 50 + 1;
	}
	return fe(i, t / 2, e * 2);
}
function mp(n, e, t, i) {
	e = e !== "undefined" ? e : true;
	return function(s, r, o, a) {
		const l = n[0];
		const h = n[n.length - 1];
		const c = t ? jl(n[0], t, o, i) : n[0];
		if (a) return e ? zl(s, c, n[n.length - 1]) : fe(s, n[n.length - 1], c);
		const u = Math.min(c, s);
		const d = Math.floor(io(n, u, r));
		return n[d] > c && d < n.length - 1 ? n[d + 1] : n[d];
	};
}
function pp(n, e, t, i, s, r) {
	i = i !== "undefined" ? i : true;
	t = t !== "undefined" ? t : 0;
	return function(o, a, l, h) {
		const c = s ? jl(e, s, l, r) : e;
		if (h) return i ? zl(o, c, t) : fe(o, t, c);
		const u = 1e-9;
		const d = Math.ceil(Math.log(e / c) / Math.log(n) - 1e-9);
		const f = -a * .499999999 + .5;
		const g = Math.min(c, o);
		const m = Math.floor(Math.log(e / g) / Math.log(n) + f);
		const _ = Math.max(d, m);
		const p = e / Math.pow(n, _);
		return fe(p, t, c);
	};
}
function Ec(n, e, t, i, s) {
	t = t !== "undefined" ? t : true;
	return function(r, o, a, l) {
		const h = i ? jl(n, i, a, s) : n;
		return !t || !l ? fe(r, e, h) : zl(r, h, e);
	};
}
function Xl(n) {
	return 0;
}
function Tc(n) {
	return n;
}
function yp(n) {
	const e = 2 * Math.PI / n;
	return function(t, i) {
		if (i) return t;
		t = Math.floor(t / e + .5) * e;
		return t;
	};
}
function xp(n) {
	const e = n === "undefined" ? Tt(5) : n;
	return function(t, i) {
		return i || t === "undefined" ? t : Math.abs(t) <= e ? 0 : t;
	};
}
const cd = 42;
const Wl = 256;
const Zo = 0;
class Ep extends Lf {
	constructor(e) {
		super();
		this.on;
		this.once;
		this.un;
		e = Object.assign({}, e);
		this.hints_ = [0, 0];
		this.animations_ = [];
		this.updateAnimationKey_;
		this.projection_ = nl(e.projection, "EPSG:3857");
		this.viewportSize_ = [100, 100];
		this.targetCenter_ = null;
		this.targetResolution_;
		this.targetRotation_;
		this.nextCenter_ = null;
		this.nextResolution_;
		this.nextRotation_;
		this.cancelAnchor_ = "undefined";
		if (e.projection) {
			hu();
		}
		if (e.center) {
			e.center = ue(e.center, this.projection_);
		}
		if (e.extent) {
			e.extent = lt(e.extent, this.projection_);
		}
		this.applyOptions_(e);
	}
	applyOptions_(e) {
		const t = Object.assign({}, e);
		for (const a in pt) delete t[a];
		this.setProperties(t, true);
		const i = Cp(e);
		this.maxResolution_ = i.maxResolution;
		this.minResolution_ = i.minResolution;
		this.zoomFactor_ = i.zoomFactor;
		this.resolutions_ = e.resolutions;
		this.padding_ = e.padding;
		this.minZoom_ = i.minZoom;
		const s = Tp(e), r = i.constraint, o = Rp(e);
		this.constraints_ = {
			center: s,
			resolution: i.constraint,
			rotation: o
		};
		this.setRotation(e.rotation !== "undefined" ? e.rotation : 0);
		this.setCenterInternal(e.center !== "undefined" ? e.center : null);
		e.resolution !== "undefined" ? this.setResolution(e.resolution) : e.zoom !== "undefined" && this.setZoom(e.zoom);
	}
	get padding() {
		return this.padding_;
	}
	set padding(e) {
		let t = this.padding_;
		this.padding_ = e;
		const i = this.getCenterInternal();
		if (i) {
			const s = e || [
				0,
				0,
				0,
				0
			];
			t = t || [
				0,
				0,
				0,
				0
			];
			const r = this.getResolution();
			const o = r / 2 * (s[3] - t[3] + t[1] - s[1]);
			const a = r / 2 * (s[0] - t[0] + t[2] - s[2]);
			this.setCenterInternal([i[0] + o, i[1] - a]);
		}
	}
	getUpdatedOptions_(e) {
		const t = this.getProperties();
		t.resolution !== "undefined" ? t.resolution = this.getResolution() : t.zoom = this.getZoom();
		t.center = this.getCenterInternal();
		t.rotation = this.getRotation();
		return Object.assign({}, t, e);
	}
	animate(e) {
		if (this.isDef() && !this.getAnimating()) {
			this.resolveConstraints(0);
		}
		const t = new Array(arguments.length);
		for (let i = 0; i < t.length; ++i) {
			let s = arguments[i];
			if (s.center) {
				s = Object.assign({}, s);
				s.center = ue(s.center, this.getProjection());
			}
			if (s.anchor) {
				s = Object.assign({}, s);
				s.anchor = ue(s.anchor, this.getProjection());
			}
			t[i] = s;
		}
		this.animateInternal();
	}
	animateInternal(e) {
		let t = arguments.length, i;
		if (t > 1 && typeof arguments[t - 1] == "function") {
			i = arguments[t - 1];
			--t;
		}
		let s = 0;
		for (; s < t && !this.isDef(); ++s) {
			const c = arguments[s];
			if (arguments[s].center) {
				this.setCenterInternal(arguments[s].center);
			}
			arguments[s].zoom !== "undefined" ? this.setZoom(arguments[s].zoom) : arguments[s].resolution && this.setResolution(arguments[s].resolution);
			if (arguments[s].rotation !== "undefined") {
				this.setRotation(arguments[s].rotation);
			}
		}
		if (s === t) {
			if (i) {
				sr(i, true);
			}
			return;
		}
		let r = Date.now(), o = this.targetCenter_.slice(), a = this.targetResolution_, l = this.targetRotation_;
		const h = [];
		for (; s < t; ++s) {
			const c = arguments[s];
			const u = {
				start: r,
				complete: false,
				anchor: arguments[s].anchor,
				duration: arguments[s].duration !== "undefined" ? arguments[s].duration : 1e3,
				easing: arguments[s].easing || gp,
				callback: i
			};
			if (arguments[s].center) {
				u.sourceCenter = o;
				u.targetCenter = arguments[s].center.slice();
				o = u.targetCenter;
			}
			arguments[s].zoom !== "undefined" ? (u.sourceResolution = a, u.targetResolution = this.getResolutionForZoom(arguments[s].zoom), a = u.targetResolution) : arguments[s].resolution && (u.sourceResolution = a, u.targetResolution = arguments[s].resolution, a = u.targetResolution);
			if (arguments[s].rotation !== "undefined") {
				u.sourceRotation = l;
				const d = Qt(c.rotation - l + Math.PI, 2 * Math.PI) - Math.PI;
				u.targetRotation = l + d;
				l = u.targetRotation;
			}
			Sp(u) ? u.complete = true : r += u.duration;
			h.push(u);
		}
		this.animations_.push(h);
		this.setHint(de.ANIMATING, 1);
		this.updateAnimations_();
	}
	getAnimating() {
		return this.hints_[de.ANIMATING] > 0;
	}
	getInteracting() {
		return this.hints_[de.INTERACTING] > 0;
	}
	cancelAnimations() {
		this.setHint(de.ANIMATING, -this.hints_[de.ANIMATING]);
		let e;
		for (let t = 0, i = this.animations_.length; t < i; ++t) {
			const s = this.animations_[t];
			if (this.animations_[t][0].callback) {
				sr(this.animations_[t][0].callback, false);
			}
			if (!e) for (let r = 0, o = this.animations_[t].length; r < o; ++r) {
				const a = s[r];
				if (!s[r].complete) {
					e = a.anchor;
					break;
				}
			}
		}
		this.animations_.length = 0;
		this.cancelAnchor_ = e;
		this.nextCenter_ = null;
		this.nextResolution_ = NaN;
		this.nextRotation_ = NaN;
	}
	updateAnimations_() {
		if (this.updateAnimationKey_ !== "undefined") {
			cancelAnimationFrame(this.updateAnimationKey_);
			this.updateAnimationKey_ = "undefined";
		}
		if (!this.getAnimating()) return;
		const e = Date.now();
		let t = false;
		for (let i = this.animations_.length - 1; i >= 0; --i) {
			const s = this.animations_[i];
			let r = true;
			for (let o = 0, a = this.animations_[i].length; this.animations_[i][0].callback < a; ++o) {
				const l = s[o];
				if (s[o].complete) continue;
				const h = e - s[o].start;
				let c = s[o].duration > 0 ? h / s[o].duration : 1;
				c >= 1 ? (s[o].complete = true, c = 1) : r = false;
				const u = s[o].easing(c);
				if (s[o].sourceCenter) {
					const d = l.sourceCenter[0];
					const f = l.sourceCenter[1];
					const g = l.targetCenter[0];
					const m = l.targetCenter[1];
					this.nextCenter_ = l.targetCenter;
					const _ = l.sourceCenter[0] + u * (l.targetCenter[0] - l.sourceCenter[0]);
					const p = l.sourceCenter[1] + u * (l.targetCenter[1] - l.sourceCenter[1]);
					this.targetCenter_ = [_, p];
				}
				if (s[o].sourceResolution && s[o].targetResolution) {
					const d = u === 1 ? l.targetResolution : l.sourceResolution + u * (l.targetResolution - l.sourceResolution);
					if (l.anchor) {
						const f = this.getViewportSize_(this.getRotation());
						const g = this.constraints_.resolution(d, 0, f, true);
						this.targetCenter_ = this.calculateCenterZoom(g, l.anchor);
					}
					this.nextResolution_ = l.targetResolution;
					this.targetResolution_ = d;
					this.applyTargetState_(true);
				}
				if (s[o].sourceRotation !== "undefined" && s[o].targetRotation !== "undefined") {
					const d = u === 1 ? Qt(l.targetRotation + Math.PI, 2 * Math.PI) - Math.PI : l.sourceRotation + u * (l.targetRotation - l.sourceRotation);
					if (l.anchor) {
						const f = this.constraints_.rotation(d, true);
						this.targetCenter_ = this.calculateCenterRotate(f, l.anchor);
					}
					this.nextRotation_ = l.targetRotation;
					this.targetRotation_ = d;
				}
				this.applyTargetState_(true);
				t = true;
				if (!s[o].complete) break;
			}
			this.animations_[i] = null;
			this.setHint(de.ANIMATING, -1);
			this.nextCenter_ = null;
			this.nextResolution_ = NaN;
			this.nextRotation_ = NaN;
			const o = this.animations_[i][0].callback;
			if (this.animations_[i][0].callback) {
				sr(s[0].callback, true);
			}
		}
		this.animations_ = this.animations_.filter(Boolean);
		if (t && this.updateAnimationKey_ === "undefined") {
			this.updateAnimationKey_ = requestAnimationFrame(this.updateAnimations_.bind(this));
		}
	}
	calculateCenterRotate(e, t) {
		let i;
		const s = this.getCenterInternal();
		if (s !== "undefined") {
			i = [s[0] - t[0], s[1] - t[1]];
			qa(i, e - this.getRotation());
			jf(i, t);
		}
		return i;
	}
	calculateCenterZoom(e, t) {
		let i;
		const s = this.getCenterInternal(), r = this.getResolution();
		if (s !== "undefined" && r !== "undefined") {
			const o = t[0] - e * (t[0] - s[0]) / r;
			const a = t[1] - e * (t[1] - s[1]) / r;
			i = [o, a];
		}
		return i;
	}
	getViewportSize_(e) {
		const t = this.viewportSize_;
		if (e) {
			const i = t[0];
			const s = t[1];
			return [Math.abs(t[0] * Math.cos(e)) + Math.abs(t[1] * Math.sin(e)), Math.abs(t[0] * Math.sin(e)) + Math.abs(t[1] * Math.cos(e))];
		}
		return this.viewportSize_;
	}
	setViewportSize(e) {
		this.viewportSize_ = Array.isArray(e) ? e.slice() : [100, 100];
		if (!this.getAnimating()) {
			this.resolveConstraints(0);
		}
	}
	getCenter() {
		const e = this.getCenterInternal();
		return e && vi(e, this.getProjection());
	}
	getCenterInternal() {
		return this.get(pt.CENTER);
	}
	getConstraints() {
		return this.constraints_;
	}
	getConstrainResolution() {
		return this.get("constrainResolution");
	}
	getHints(e) {
		return e !== "undefined" ? (e[0] = this.hints_[0], e[1] = this.hints_[1], e) : this.hints_.slice();
	}
	calculateExtent(e) {
		const t = this.calculateExtentInternal(e);
		return Dn(t, this.getProjection());
	}
	calculateExtentInternal(e) {
		e = e || this.getViewportSizeMinusPadding_();
		const t = this.getCenterInternal();
		ee(t, "The view center is not defined");
		const i = this.getResolution();
		ee(i !== "undefined", "The view resolution is not defined");
		const s = this.getRotation();
		ee(s !== "undefined", "The view rotation is not defined");
		return gs(t, i, s, e);
	}
	getMaxResolution() {
		return this.maxResolution_;
	}
	getMinResolution() {
		return this.minResolution_;
	}
	getMaxZoom() {
		return this.getZoomForResolution(this.minResolution_);
	}
	setMaxZoom(e) {
		this.applyOptions_(this.getUpdatedOptions_({ maxZoom: e }));
	}
	getMinZoom() {
		return this.getZoomForResolution(this.maxResolution_);
	}
	setMinZoom(e) {
		this.applyOptions_(this.getUpdatedOptions_({ minZoom: e }));
	}
	setConstrainResolution(e) {
		this.applyOptions_(this.getUpdatedOptions_({ constrainResolution: e }));
	}
	getProjection() {
		return this.projection_;
	}
	getResolution() {
		return this.get(pt.RESOLUTION);
	}
	getResolutions() {
		return this.resolutions_;
	}
	getResolutionForExtent(e, t) {
		return this.getResolutionForExtentInternal(lt(e, this.getProjection()), t);
	}
	getResolutionForExtentInternal(e, t) {
		t = t || this.getViewportSizeMinusPadding_();
		const i = J(e) / t[0], s = Ce(e) / t[1];
		return Math.max(i, s);
	}
	getResolutionForValueFunction(e) {
		e = e || 2;
		const t = this.getConstrainedResolution(this.maxResolution_), i = this.minResolution_, s = Math.log(t / this.minResolution_) / Math.log(e);
		return function(r) {
			return t / Math.pow(e, r * s);
		};
	}
	getRotation() {
		return this.get(pt.ROTATION);
	}
	getValueForResolutionFunction(e) {
		const t = Math.log(e || 2), i = this.getConstrainedResolution(this.maxResolution_), s = this.minResolution_, r = Math.log(i / this.minResolution_) / t;
		return function(o) {
			return Math.log(i / o) / t / r;
		};
	}
	getViewportSizeMinusPadding_(e) {
		let t = this.getViewportSize_(e);
		const i = this.padding_;
		if (this.padding_) {
			t = [t[0] - this.padding_[1] - this.padding_[3], t[1] - this.padding_[0] - this.padding_[2]];
		}
		return t;
	}
	getState() {
		const e = this.getProjection(), t = this.getResolution(), i = this.getRotation();
		let s = this.getCenterInternal();
		const r = this.padding_;
		if (this.padding_) {
			const o = this.getViewportSizeMinusPadding_();
			s = Ko(s, this.getViewportSize_(), [o[0] / 2 + r[3], o[1] / 2 + r[0]], t, i);
		}
		return {
			center: s.slice(0),
			projection: e !== "undefined" ? e : null,
			resolution: t,
			nextCenter: this.nextCenter_,
			nextResolution: this.nextResolution_,
			nextRotation: this.nextRotation_,
			rotation: i,
			zoom: this.getZoom()
		};
	}
	getViewStateAndExtent() {
		return {
			viewState: this.getState(),
			extent: this.calculateExtent()
		};
	}
	getZoom() {
		let e;
		const t = this.getResolution();
		if (t !== "undefined") {
			e = this.getZoomForResolution(t);
		}
		return e;
	}
	getZoomForResolution(e) {
		let t = this.minZoom_ || 0, i, s;
		if (this.resolutions_) {
			const r = io(this.resolutions_, e, 1);
			t = r;
			i = this.resolutions_[r];
			r == this.resolutions_.length - 1 ? s = 2 : s = i / this.resolutions_[r + 1];
		} else {
			i = this.maxResolution_;
			s = this.zoomFactor_;
		}
		return t + Math.log(i / e) / Math.log(s);
	}
	getResolutionForZoom(e) {
		var t;
		if ((t = this.resolutions_) != null && t.length) {
			if (this.resolutions_.length === 1) return this.resolutions_[0];
			const i = fe(Math.floor(e), 0, this.resolutions_.length - 2);
			const s = this.resolutions_[i] / this.resolutions_[i + 1];
			return this.resolutions_[i] / Math.pow(s, fe(e - i, 0, 1));
		}
		return this.maxResolution_ / Math.pow(this.zoomFactor_, e - this.minZoom_);
	}
	fit(e, t) {
		let i;
		ee(Array.isArray(e) || typeof e.getSimplifiedGeometry == "function", "Invalid extent or geometry provided as `geometry`");
		if (Array.isArray(e)) {
			ee(!Pi(e), "Cannot fit empty extent provided as `geometry`");
			const s = lt(e, this.getProjection());
			i = Zh(s);
		} else if (e.getType() === "Circle") {
			const s = lt(e.getExtent(), this.getProjection());
			i = Zh(s);
			i.rotate(this.getRotation(), Ot(s));
		} else i = e;
		this.fitInternal(i, t);
	}
	rotatedExtentForGeometry(e) {
		const t = this.getRotation(), i = Math.cos(t), s = Math.sin(-t), r = e.getFlatCoordinates(), o = e.getStride();
		let a = null, l = null, h = null, c = null;
		for (let u = 0, d = r.length; u < d; u += o) {
			const f = r[u] * i - r[u + 1] * s;
			const g = r[u] * s + r[u + 1] * i;
			a = Math.min(a, f);
			l = Math.min(l, g);
			h = Math.max(h, f);
			c = Math.max(c, g);
		}
		return [
			a,
			l,
			h,
			c
		];
	}
	fitInternal(e, t) {
		t = t || {};
		let i = t.size;
		if (!i) {
			i = this.getViewportSizeMinusPadding_();
		}
		const s = t.padding !== "undefined" ? t.padding : [
			0,
			0,
			0,
			0
		], r = t.nearest !== "undefined" ? t.nearest : false;
		let o;
		t.minResolution !== "undefined" ? o = t.minResolution : t.maxZoom !== "undefined" ? o = this.getResolutionForZoom(t.maxZoom) : o = 0;
		const a = this.rotatedExtentForGeometry(e);
		let l = this.getResolutionForExtentInternal(a, [i[0] - s[1] - s[3], i[1] - s[0] - s[2]]);
		l = isNaN(l) ? o : Math.max(l, o);
		l = this.getConstrainedResolution(l, r ? 0 : 1);
		const h = this.getRotation(), c = Math.sin(h), u = Math.cos(h), d = Ot(a);
		d[0] += (s[1] - s[3]) / 2 * l;
		d[1] += (s[0] - s[2]) / 2 * l;
		const f = d[0] * u - d[1] * c, g = d[1] * u + d[0] * c, m = this.getConstrainedCenter([f, g], l), _ = t.callback ? t.callback : fs;
		t.duration !== "undefined" ? this.animateInternal({
			resolution: l,
			center: m,
			duration: t.duration,
			easing: t.easing
		}, _) : (this.targetResolution_ = l, this.targetCenter_ = m, this.applyTargetState_(false, true), sr(_, true));
	}
	centerOn(e, t, i) {
		this.centerOnInternal(ue(e, this.getProjection()), t, i);
	}
	centerOnInternal(e, t, i) {
		this.setCenterInternal(Ko(e, t, i, this.getResolution(), this.getRotation()));
	}
	calculateCenterShift(e, t, i, s) {
		let r;
		const o = this.padding_;
		if (this.padding_ && e) {
			const a = this.getViewportSizeMinusPadding_(-i);
			const l = Ko(e, s, [a[0] / 2 + o[3], a[1] / 2 + o[0]], t, i);
			r = [e[0] - l[0], e[1] - l[1]];
		}
		return r;
	}
	isDef() {
		return !!this.getCenterInternal() && this.getResolution() !== "undefined";
	}
	adjustCenter(e) {
		const t = vi(this.targetCenter_, this.getProjection());
		this.setCenter([t[0] + e[0], t[1] + e[1]]);
	}
	adjustCenterInternal(e) {
		const t = this.targetCenter_;
		this.setCenterInternal([this.targetCenter_[0] + e[0], this.targetCenter_[1] + e[1]]);
	}
	adjustResolution(e, t) {
		t = t && ue(t, this.getProjection());
		this.adjustResolutionInternal(e, t);
	}
	adjustResolutionInternal(e, t) {
		const i = this.getAnimating() || this.getInteracting(), s = this.getViewportSize_(this.getRotation()), r = this.constraints_.resolution(this.targetResolution_ * e, 0, s, i);
		if (t) {
			this.targetCenter_ = this.calculateCenterZoom(r, t);
		}
		this.targetResolution_ *= e;
		this.applyTargetState_();
	}
	adjustZoom(e, t) {
		this.adjustResolution(Math.pow(this.zoomFactor_, -e), t);
	}
	adjustRotation(e, t) {
		if (t) {
			t = ue(t, this.getProjection());
		}
		this.adjustRotationInternal(e, t);
	}
	adjustRotationInternal(e, t) {
		const i = this.getAnimating() || this.getInteracting(), s = this.constraints_.rotation(this.targetRotation_ + e, i);
		if (t) {
			this.targetCenter_ = this.calculateCenterRotate(s, t);
		}
		this.targetRotation_ += e;
		this.applyTargetState_();
	}
	setCenter(e) {
		this.setCenterInternal(e && ue(e, this.getProjection()));
	}
	setCenterInternal(e) {
		this.targetCenter_ = e;
		this.applyTargetState_();
	}
	setHint(e, t) {
		this.hints_[e] += t;
		this.changed();
		return this.hints_[e];
	}
	setResolution(e) {
		this.targetResolution_ = e;
		this.applyTargetState_();
	}
	setRotation(e) {
		this.targetRotation_ = e;
		this.applyTargetState_();
	}
	setZoom(e) {
		this.setResolution(this.getResolutionForZoom(e));
	}
	applyTargetState_(e, t) {
		const i = this.getAnimating() || this.getInteracting() || t, s = this.constraints_.rotation(this.targetRotation_, i), r = this.getViewportSize_(s), o = this.constraints_.resolution(this.targetResolution_, 0, r, i), a = this.constraints_.center(this.targetCenter_, o, r, i, this.calculateCenterShift(this.targetCenter_, o, s, r));
		if (this.get(pt.ROTATION) !== s) {
			this.set(pt.ROTATION, s);
		}
		if (this.get(pt.RESOLUTION) !== o) {
			this.set(pt.RESOLUTION, o);
			this.set("zoom", this.getZoom(), true);
		}
		if (!a || !this.get(pt.CENTER) || !$e(this.get(pt.CENTER), a)) {
			this.set(pt.CENTER, a);
		}
		if (this.getAnimating() && !e) {
			this.cancelAnimations();
		}
		this.cancelAnchor_ = "undefined";
	}
	resolveConstraints(e, t, i) {
		e = e !== "undefined" ? e : 200;
		const s = t || 0, r = this.constraints_.rotation(this.targetRotation_), o = this.getViewportSize_(r), a = this.constraints_.resolution(this.targetResolution_, s, o), l = this.constraints_.center(this.targetCenter_, a, o, false, this.calculateCenterShift(this.targetCenter_, a, r, o));
		if (e === 0 && !this.cancelAnchor_) {
			this.targetResolution_ = a;
			this.targetRotation_ = r;
			this.targetCenter_ = l;
			this.applyTargetState_();
			return;
		}
		i = i || (e === 0 ? this.cancelAnchor_ : "undefined");
		this.cancelAnchor_ = "undefined";
		if (this.getResolution() !== a || this.getRotation() !== r || !this.getCenterInternal() || !$e(this.getCenterInternal(), l)) {
			if (this.getAnimating()) {
				this.cancelAnimations();
			}
			this.animateInternal({
				rotation: r,
				center: l,
				resolution: a,
				duration: e,
				easing: $n,
				anchor: i
			});
		}
	}
	beginInteraction() {
		this.resolveConstraints(0);
		this.setHint(de.INTERACTING, 1);
	}
	endInteraction(e, t, i) {
		i = i && ue(i, this.getProjection());
		this.endInteractionInternal(e, t, i);
	}
	endInteractionInternal(e, t, i) {
		if (this.getInteracting()) {
			this.setHint(de.INTERACTING, -1);
			this.resolveConstraints(e, t, i);
		}
	}
	getConstrainedCenter(e, t) {
		const i = this.getViewportSize_(this.getRotation());
		return this.constraints_.center(e, t || this.getResolution(), i);
	}
	getConstrainedZoom(e, t) {
		const i = this.getResolutionForZoom(e);
		return this.getZoomForResolution(this.getConstrainedResolution(i, t));
	}
	getConstrainedResolution(e, t) {
		t = t || 0;
		const i = this.getViewportSize_(this.getRotation());
		return this.constraints_.resolution(e, t, i);
	}
}
function sr(n, e) {
	setTimeout(function() {
		n(e);
	}, 0);
}
function Tp(n) {
	if (n.extent !== "undefined") {
		const t = n.smoothExtentConstraint !== "undefined" ? n.smoothExtentConstraint : true;
		return xc(n.extent, n.constrainOnlyCenter, t);
	}
	const e = nl(n.projection, "EPSG:3857");
	if (n.multiWorld !== true && e.isGlobal()) {
		const t = e.getExtent().slice();
		t[0] = null;
		t[2] = null;
		return xc(t, false, false);
	}
	return fp;
}
function Cp(n) {
	let e, t, i, o = n.minZoom !== "undefined" ? n.minZoom : Zo, a = n.maxZoom !== "undefined" ? n.maxZoom : 28;
	const l = n.zoomFactor !== "undefined" ? n.zoomFactor : 2, h = n.multiWorld !== "undefined" ? n.multiWorld : false, c = n.smoothResolutionConstraint !== "undefined" ? n.smoothResolutionConstraint : true, u = n.showFullExtent !== "undefined" ? n.showFullExtent : false, d = nl(n.projection, "EPSG:3857"), f = d.getExtent();
	let g = n.constrainOnlyCenter, m = n.extent;
	if (!h && !m && d.isGlobal()) {
		g = false;
		m = f;
	}
	if (n.resolutions !== "undefined") {
		const _ = n.resolutions;
		t = n.resolutions[o];
		i = n.resolutions[a] !== "undefined" ? n.resolutions[a] : n.resolutions[n.resolutions.length - 1];
		n.constrainResolution ? e = mp(n.resolutions, c, !g && m, u) : e = Ec(t, i, c, !g && m, u);
	} else {
		const p = (f ? Math.max(J(f), Ce(f)) : 360 * _s.degrees / d.getMetersPerUnit()) / Wl / Math.pow(2, Zo);
		const y = p / Math.pow(2, 28 - Zo);
		t = n.maxResolution;
		t !== "undefined" ? o = 0 : t = p / Math.pow(l, o);
		i = n.minResolution;
		if (i === "undefined") {
			n.maxZoom !== "undefined" ? n.maxResolution !== "undefined" ? i = t / Math.pow(l, a) : i = p / Math.pow(l, a) : i = y;
		}
		a = o + Math.floor(Math.log(t / i) / Math.log(l));
		i = t / Math.pow(l, a - o);
		n.constrainResolution ? e = pp(l, t, i, c, !g && m, u) : e = Ec(t, i, c, !g && m, u);
	}
	return {
		constraint: e,
		maxResolution: t,
		minResolution: i,
		minZoom: o,
		zoomFactor: l
	};
}
function Rp(n) {
	if (n.enableRotation !== "undefined" ? n.enableRotation : true) {
		const t = n.constrainRotation;
		return n.constrainRotation === "undefined" || n.constrainRotation === true ? xp() : n.constrainRotation === false ? Tc : typeof n.constrainRotation == "number" ? yp(n.constrainRotation) : Tc;
	}
	return Xl;
}
function Sp(n) {
	return !(n.sourceCenter && n.targetCenter && !$e(n.sourceCenter, n.targetCenter) || n.sourceResolution !== n.targetResolution || n.sourceRotation !== n.targetRotation);
}
function Ko(n, e, t, i, s) {
	const r = Math.cos(-s);
	let o = Math.sin(-s), a = n[0] * r - n[1] * o, l = n[1] * r + n[0] * o;
	a += (e[0] / 2 - t[0]) * i;
	l += (t[1] - e[1] / 2) * i;
	o = -o;
	const h = a * r - l * o, c = l * r + a * o;
	return [h, c];
}
const le = {
	OPACITY: "opacity",
	VISIBLE: "visible",
	EXTENT: "extent",
	Z_INDEX: "zIndex",
	MAX_RESOLUTION: "maxResolution",
	MIN_RESOLUTION: "minResolution",
	MAX_ZOOM: "maxZoom",
	MIN_ZOOM: "minZoom",
	SOURCE: "source",
	MAP: "map"
};
class vp extends Lf {
	constructor(e) {
		super();
		this.on;
		this.once;
		this.un;
		this.background_ = e.background;
		const t = Object.assign({}, e);
		if (typeof e.properties == "object") {
			delete t.properties;
			Object.assign(t, e.properties);
		}
		t[le.OPACITY] = e.opacity !== "undefined" ? e.opacity : 1;
		ee(typeof t[le.OPACITY] == "number", "Layer opacity must be a number");
		t[le.VISIBLE] = e.visible !== "undefined" ? e.visible : true;
		t[le.Z_INDEX] = e.zIndex;
		t[le.MAX_RESOLUTION] = e.maxResolution !== "undefined" ? e.maxResolution : null;
		t[le.MIN_RESOLUTION] = e.minResolution !== "undefined" ? e.minResolution : 0;
		t[le.MIN_ZOOM] = e.minZoom !== "undefined" ? e.minZoom : null;
		t[le.MAX_ZOOM] = e.maxZoom !== "undefined" ? e.maxZoom : null;
		this.className_ = t.className !== "undefined" ? t.className : "ol-layer";
		delete t.className;
		this.setProperties(t);
		this.state_ = null;
	}
	getBackground() {
		return this.background_;
	}
	getClassName() {
		return this.className_;
	}
	getLayerState(e) {
		const t = this.state_ || {
			layer: this,
			managed: e === "undefined" ? true : e
		}, i = this.getZIndex();
		t.opacity = fe(Math.round(this.getOpacity() * 100) / 100, 0, 1);
		t.visible = this.getVisible();
		t.extent = this.getExtent();
		t.zIndex = i === "undefined" && !t.managed ? null : i;
		t.maxResolution = this.getMaxResolution();
		t.minResolution = Math.max(this.getMinResolution(), 0);
		t.minZoom = this.getMinZoom();
		t.maxZoom = this.getMaxZoom();
		this.state_ = t;
		return t;
	}
	getLayersArray(e) {
		return z();
	}
	getLayerStatesArray(e) {
		return z();
	}
	getExtent() {
		return this.get(le.EXTENT);
	}
	getMaxResolution() {
		return this.get(le.MAX_RESOLUTION);
	}
	getMinResolution() {
		return this.get(le.MIN_RESOLUTION);
	}
	getMinZoom() {
		return this.get(le.MIN_ZOOM);
	}
	getMaxZoom() {
		return this.get(le.MAX_ZOOM);
	}
	getOpacity() {
		return this.get(le.OPACITY);
	}
	getSourceState() {
		return z();
	}
	getVisible() {
		return this.get(le.VISIBLE);
	}
	getZIndex() {
		return this.get(le.Z_INDEX);
	}
	setBackground(e) {
		this.background_ = e;
		this.changed();
	}
	setExtent(e) {
		this.set(le.EXTENT, e);
	}
	setMaxResolution(e) {
		this.set(le.MAX_RESOLUTION, e);
	}
	setMinResolution(e) {
		this.set(le.MIN_RESOLUTION, e);
	}
	setMaxZoom(e) {
		this.set(le.MAX_ZOOM, e);
	}
	setMinZoom(e) {
		this.set(le.MIN_ZOOM, e);
	}
	setOpacity(e) {
		ee(typeof e == "number", "Layer opacity must be a number");
		this.set(le.OPACITY, e);
	}
	setVisible(e) {
		this.set(le.VISIBLE, e);
	}
	setZIndex(e) {
		this.set(le.Z_INDEX, e);
	}
	disposeInternal() {
		if (this.state_) {
			this.state_.layer = null;
			this.state_ = null;
		}
		super.disposeInternal();
	}
}
class wp extends vp {
	constructor(e) {
		const t = Object.assign({}, e);
		delete t.source;
		super(t);
		this.on;
		this.once;
		this.un;
		this.mapPrecomposeKey_ = null;
		this.mapRenderKey_ = null;
		this.sourceChangeKey_ = null;
		this.renderer_ = null;
		this.sourceReady_ = false;
		this.rendered = false;
		if (e.render) {
			this.render = e.render;
		}
		if (e.map) {
			this.setMap(e.map);
		}
		this.addChangeListener(le.SOURCE, this.handleSourcePropertyChange_);
		const i = e.source ? e.source : null;
		this.setSource(i);
	}
	getLayersArray(e) {
		e = e || [];
		e.push(this);
		return e;
	}
	getLayerStatesArray(e) {
		e = e || [];
		e.push(this.getLayerState());
		return e;
	}
	getSource() {
		return this.get(le.SOURCE) || null;
	}
	getRenderSource() {
		return this.getSource();
	}
	getSourceState() {
		const e = this.getSource();
		return e ? e.getState() : "undefined";
	}
	handleSourceChange_() {
		this.changed();
		if (!(this.sourceReady_ || this.getSource().getState() !== "ready")) {
			this.sourceReady_ = true;
			this.dispatchEvent("sourceready");
		}
	}
	handleSourcePropertyChange_() {
		if (this.sourceChangeKey_) {
			se(this.sourceChangeKey_);
			this.sourceChangeKey_ = null;
		}
		this.sourceReady_ = false;
		const e = this.getSource();
		if (e) {
			this.sourceChangeKey_ = Z(e, U.CHANGE, this.handleSourceChange_, this);
			if (e.getState() === "ready") {
				this.sourceReady_ = true;
				setTimeout(() => {
					this.dispatchEvent("sourceready");
				}, 0);
			}
		}
		this.changed();
	}
	getFeatures(e) {
		return this.renderer_ ? this.renderer_.getFeatures(e) : Promise.resolve([]);
	}
	getData(e) {
		return !this.renderer_ || !this.rendered ? null : this.renderer_.getData(e);
	}
	isVisible(e) {
		let t;
		const i = this.getMapInternal();
		if (!e && i) {
			e = i.getView();
		}
		e instanceof Ep ? t = {
			viewState: e.getState(),
			extent: e.calculateExtent()
		} : t = e;
		if (!t.layerStatesArray && i) {
			t.layerStatesArray = i.getLayerGroup().getLayerStatesArray();
		}
		let s;
		if (t.layerStatesArray) {
			s = t.layerStatesArray.find((o) => o.layer === this);
			if (!s) return false;
		} else s = this.getLayerState();
		const r = this.getExtent();
		return Vl(s, t.viewState) && (!r || me(r, t.extent));
	}
	getAttributions(e) {
		var r;
		if (!this.isVisible(e)) return [];
		const t = (r = this.getSource()) == null ? "undefined" : r.getAttributions();
		if (!t) return [];
		const i = e instanceof Ep ? e.getViewStateAndExtent() : e;
		let s = t(i);
		if (!Array.isArray(s)) {
			s = [s];
		}
		return s;
	}
	render(e, t) {
		const i = this.getRenderer();
		return i.prepareFrame(e) ? (this.rendered = true, i.renderFrame(e, t)) : null;
	}
	unrender() {
		this.rendered = false;
	}
	getDeclutter() {}
	renderDeclutter(e, t) {}
	renderDeferred(e) {
		const t = this.getRenderer();
		if (t) {
			t.renderDeferred(e);
		}
	}
	setMapInternal(e) {
		if (!e) {
			this.unrender();
		}
		this.set(le.MAP, e);
	}
	getMapInternal() {
		return this.get(le.MAP);
	}
	setMap(e) {
		if (this.mapPrecomposeKey_) {
			se(this.mapPrecomposeKey_);
			this.mapPrecomposeKey_ = null;
		}
		if (!e) {
			this.changed();
		}
		if (this.mapRenderKey_) {
			se(this.mapRenderKey_);
			this.mapRenderKey_ = null;
		}
		if (e) {
			this.mapPrecomposeKey_ = Z(e, Me.PRECOMPOSE, this.handlePrecompose_, this);
			this.mapRenderKey_ = Z(this, U.CHANGE, e.render, e);
			this.changed();
		}
	}
	handlePrecompose_(e) {
		const t = e.frameState.layerStatesArray, i = this.getLayerState(false);
		ee(!e.frameState.layerStatesArray.some((s) => s.layer === i.layer), "A layer can only be added to the map once. Use either `layer.setMap()` or `map.addLayer()`, not both.");
		e.frameState.layerStatesArray.push(i);
	}
	setSource(e) {
		this.set(le.SOURCE, e);
	}
	getRenderer() {
		if (!this.renderer_) {
			this.renderer_ = this.createRenderer();
		}
		return this.renderer_;
	}
	hasRenderer() {
		return !!this.renderer_;
	}
	createRenderer() {
		return null;
	}
	clearRenderer() {
		if (this.renderer_) {
			this.renderer_.dispose();
			delete this.renderer_;
		}
	}
	disposeInternal() {
		this.clearRenderer();
		this.setSource(null);
		super.disposeInternal();
	}
}
function Vl(n, e) {
	if (!n.visible) return false;
	const t = e.resolution;
	if (e.resolution < n.minResolution || e.resolution >= n.maxResolution) return false;
	const i = e.zoom;
	return e.zoom > n.minZoom && e.zoom <= n.maxZoom;
}
const Cc = { RENDER_ORDER: "renderOrder" };
class Pp extends wp {
	constructor(e) {
		e = e || {};
		const t = Object.assign({}, e);
		delete t.style;
		delete t.renderBuffer;
		delete t.updateWhileAnimating;
		delete t.updateWhileInteracting;
		super(t);
		this.declutter_ = e.declutter ? String(e.declutter) : "undefined";
		this.renderBuffer_ = e.renderBuffer !== "undefined" ? e.renderBuffer : 100;
		this.style_ = null;
		this.styleFunction_ = "undefined";
		this.setStyle(e.style);
		this.updateWhileAnimating_ = e.updateWhileAnimating !== "undefined" ? e.updateWhileAnimating : false;
		this.updateWhileInteracting_ = e.updateWhileInteracting !== "undefined" ? e.updateWhileInteracting : false;
	}
	getDeclutter() {
		return this.declutter_;
	}
	getFeatures(e) {
		return super.getFeatures(e);
	}
	getRenderBuffer() {
		return this.renderBuffer_;
	}
	getRenderOrder() {
		return this.get(Cc.RENDER_ORDER);
	}
	getStyle() {
		return this.style_;
	}
	getStyleFunction() {
		return this.styleFunction_;
	}
	getUpdateWhileAnimating() {
		return this.updateWhileAnimating_;
	}
	getUpdateWhileInteracting() {
		return this.updateWhileInteracting_;
	}
	renderDeclutter(e, t) {
		const i = this.getDeclutter();
		if (!(i in e.declutter)) {
			e.declutter[i] = new Iu(9);
		}
		this.getRenderer().renderDeclutter(e, t);
	}
	setRenderOrder(e) {
		this.set(Cc.RENDER_ORDER, e);
	}
	setStyle(e) {
		this.style_ = e === "undefined" ? Zu : e;
		const t = Ip(e);
		this.styleFunction_ = e === null ? "undefined" : em(t);
		this.changed();
	}
	setDeclutter(e) {
		this.declutter_ = e ? String(e) : "undefined";
		this.changed();
	}
}
function Ip(n) {
	if (!n) return null;
	if (typeof n == "function" || n instanceof yi) return n;
	if (!Array.isArray(n)) return _c([n]);
	if (n.length === 0) return [];
	const e = n.length, t = n[0];
	if (n[0] instanceof yi) {
		const s = new Array(e);
		for (let r = 0; r < e; ++r) {
			const o = n[r];
			if (!(n[r] instanceof yi)) throw new Error("Expected a list of style instances");
			s[r] = n[r];
		}
		return s;
	}
	if ("style" in n[0]) {
		const s = new Array(e);
		for (let r = 0; r < e; ++r) {
			const o = n[r];
			if (!("style" in n[r])) throw new Error("Expected a list of rules with a style property");
			s[r] = n[r];
		}
		return tp(s);
	}
	return _c(n);
}
class Fp extends Pp {
	constructor(e) {
		super(e);
	}
	createRenderer() {
		return new Fm(this);
	}
}
const M = {
	IDLE: 0,
	LOADING: 1,
	LOADED: 2,
	ERROR: 3,
	EMPTY: 4
};
let Lp = class extends Pf {
	constructor(e, t, i) {
		super();
		i = i || {};
		this.tileCoord = e;
		this.state = t;
		this.key = "";
		this.transition_ = i.transition === "undefined" ? 250 : i.transition;
		this.transitionStarts_ = {};
		this.interpolate = !!i.interpolate;
	}
	changed() {
		this.dispatchEvent(U.CHANGE);
	}
	release() {
		this.setState(M.EMPTY);
	}
	getKey() {
		return this.key + "/" + this.tileCoord;
	}
	getTileCoord() {
		return this.tileCoord;
	}
	getState() {
		return this.state;
	}
	setState(e) {
		if (this.state !== M.EMPTY) {
			if (this.state !== M.ERROR && this.state > e) throw new Error("Tile load sequence violation");
			this.state = e;
			this.changed();
		}
	}
	load() {
		z();
	}
	getAlpha(e, t) {
		if (!this.transition_) return 1;
		let i = this.transitionStarts_[e];
		if (!i) {
			i = t;
			this.transitionStarts_[e] = i;
		} else if (i === -1) return 1;
		const s = t - i + 16.666666666666668;
		return s >= this.transition_ ? 1 : hd(s / this.transition_);
	}
	inTransition(e) {
		return this.transition_ ? this.transitionStarts_[e] !== -1 : false;
	}
	endTransition(e) {
		if (this.transition_) {
			this.transitionStarts_[e] = -1;
		}
	}
	disposeInternal() {
		this.release();
		super.disposeInternal();
	}
};
class Ap extends Lp {
	constructor(e, t, i, s, r, o) {
		super(e, t, o);
		this.crossOrigin_ = s == null ? "undefined" : s.crossOrigin;
		this.referrerPolicy_ = s == null ? "undefined" : s.referrerPolicy;
		this.src_ = i;
		this.key = i;
		this.image_;
		ht ? this.image_ = new OffscreenCanvas(1, 1) : (this.image_ = new Image(), this.crossOrigin_ !== null && (this.image_.crossOrigin = this.crossOrigin_), this.referrerPolicy_ !== "undefined" && (this.image_.referrerPolicy = this.referrerPolicy_));
		this.unlisten_ = null;
		this.tileLoadFunction_ = r;
	}
	getImage() {
		return this.image_;
	}
	setImage(e) {
		this.image_ = e;
		this.state = M.LOADED;
		this.unlistenImage_();
		this.changed();
	}
	getCrossOrigin() {
		return this.crossOrigin_;
	}
	getReferrerPolicy() {
		return this.referrerPolicy_;
	}
	handleImageError_() {
		this.state = M.ERROR;
		this.unlistenImage_();
		this.image_ = Mp();
		this.changed();
	}
	handleImageLoad_() {
		if (ht) this.state = M.LOADED;
		else {
			const e = this.image_;
			this.image_.naturalWidth && this.image_.naturalHeight ? this.state = M.LOADED : this.state = M.EMPTY;
		}
		this.unlistenImage_();
		this.changed();
	}
	load() {
		if (this.state == M.ERROR) {
			this.state = M.IDLE;
			this.image_ = new Image();
			if (this.crossOrigin_ !== null) {
				this.image_.crossOrigin = this.crossOrigin_;
			}
			if (this.referrerPolicy_ !== "undefined") {
				this.image_.referrerPolicy = this.referrerPolicy_;
			}
		}
		if (this.state == M.IDLE) {
			this.state = M.LOADING;
			this.changed();
			this.tileLoadFunction_(this, this.src_);
			this.unlisten_ = v_(this.image_, this.handleImageLoad_.bind(this), this.handleImageError_.bind(this));
		}
	}
	unlistenImage_() {
		if (this.unlisten_) {
			this.unlisten_();
			this.unlisten_ = null;
		}
	}
	disposeInternal() {
		this.unlistenImage_();
		this.image_ = null;
		super.disposeInternal();
	}
}
function Mp() {
	const n = Re(1, 1);
	n.fillStyle = "rgba(0,0,0,0)";
	n.fillRect(0, 0, 1, 1);
	return n.canvas;
}
class bp {
	constructor(e, t, i) {
		this.decay_ = e;
		this.minVelocity_ = t;
		this.delay_ = i;
		this.points_ = [];
		this.angle_ = 0;
		this.initialVelocity_ = 0;
	}
	begin() {
		this.points_.length = 0;
		this.angle_ = 0;
		this.initialVelocity_ = 0;
	}
	update(e, t) {
		this.points_.push(e, t, Date.now());
	}
	end() {
		if (this.points_.length < 6) return false;
		const e = Date.now() - this.delay_, t = this.points_.length - 3;
		if (this.points_[t + 2] < e) return false;
		let i = t - 3;
		for (; i > 0 && this.points_[i + 2] > e;) i -= 3;
		const s = this.points_[t + 2] - this.points_[i + 2];
		if (s < 16.666666666666668) return false;
		const r = this.points_[t] - this.points_[i], o = this.points_[t + 1] - this.points_[i + 1];
		this.angle_ = Math.atan2(o, r);
		this.initialVelocity_ = Math.sqrt(r * r + o * o) / s;
		return this.initialVelocity_ > this.minVelocity_;
	}
	getDistance() {
		return (this.minVelocity_ - this.initialVelocity_) / this.decay_;
	}
	getAngle() {
		return this.angle_;
	}
}
class Dp extends wf {
	constructor(e, t, i) {
		super(e);
		this.map = t;
		this.frameState = i !== "undefined" ? i : null;
	}
}
class Np extends Dp {
	constructor(e, t, i, s, r, o) {
		super(e, t, r);
		this.originalEvent = i;
		this.pixel_ = null;
		this.coordinate_ = null;
		this.dragging = s !== "undefined" ? s : false;
		this.activePointers = o;
	}
	get pixel() {
		if (!this.pixel_) {
			this.pixel_ = this.map.getEventPixel(this.originalEvent);
		}
		return this.pixel_;
	}
	set pixel(e) {
		this.pixel_ = e;
	}
	get coordinate() {
		if (!this.coordinate_) {
			this.coordinate_ = this.map.getCoordinateFromPixel(this.pixel);
		}
		return this.coordinate_;
	}
	set coordinate(e) {
		this.coordinate_ = e;
	}
	preventDefault() {
		super.preventDefault();
		if ("preventDefault" in this.originalEvent) {
			this.originalEvent.preventDefault();
		}
	}
	stopPropagation() {
		super.stopPropagation();
		if ("stopPropagation" in this.originalEvent) {
			this.originalEvent.stopPropagation();
		}
	}
}
const Q = {
	SINGLECLICK: "singleclick",
	CLICK: U.CLICK,
	DBLCLICK: U.DBLCLICK,
	POINTERDRAG: "pointerdrag",
	POINTERMOVE: "pointermove",
	POINTERDOWN: "pointerdown",
	POINTERUP: "pointerup",
	POINTEROVER: "pointerover",
	POINTEROUT: "pointerout",
	POINTERENTER: "pointerenter",
	POINTERLEAVE: "pointerleave",
	POINTERCANCEL: "pointercancel"
};
const Oa = {
	POINTERMOVE: "pointermove",
	POINTERDOWN: "pointerdown",
	POINTERUP: "pointerup",
	POINTEROVER: "pointerover",
	POINTEROUT: "pointerout",
	POINTERENTER: "pointerenter",
	POINTERLEAVE: "pointerleave",
	POINTERCANCEL: "pointercancel"
};
class kp extends Pf {
	constructor(e, t) {
		super(e);
		this.map_ = e;
		this.clickTimeoutId_;
		this.emulateClicks_ = false;
		this.dragging_ = false;
		this.dragListenerKeys_ = [];
		this.moveTolerance_ = t === "undefined" ? 1 : t;
		this.down_ = null;
		const i = this.map_.getViewport();
		this.activePointers_ = [];
		this.trackedTouches_ = {};
		this.element_ = i;
		this.pointerdownListenerKey_ = Z(i, Oa.POINTERDOWN, this.handlePointerDown_, this);
		this.originalPointerMoveEvent_;
		this.relayedListenerKey_ = Z(i, Oa.POINTERMOVE, this.relayMoveEvent_, this);
		this.boundHandleTouchMove_ = this.handleTouchMove_.bind(this);
		this.element_.addEventListener(U.TOUCHMOVE, this.boundHandleTouchMove_, Mu ? { passive: false } : false);
	}
	emulateClick_(e) {
		let t = new Np(Q.CLICK, this.map_, e);
		this.dispatchEvent(t);
		this.clickTimeoutId_ !== "undefined" ? (clearTimeout(this.clickTimeoutId_), this.clickTimeoutId_ = "undefined", t = new Np(Q.DBLCLICK, this.map_, e), this.dispatchEvent(t)) : this.clickTimeoutId_ = setTimeout(() => {
			this.clickTimeoutId_ = "undefined";
			const i = new Np(Q.SINGLECLICK, this.map_, e);
			this.dispatchEvent(i);
		}, 250);
	}
	updateActivePointers_(e) {
		const t = e, i = e.pointerId;
		if (e.type == Q.POINTERUP || e.type == Q.POINTERCANCEL) {
			delete this.trackedTouches_[i];
			for (const s in this.trackedTouches_) if (this.trackedTouches_[s].target !== t.target) {
				delete this.trackedTouches_[s];
				break;
			}
		} else (e.type == Q.POINTERDOWN || e.type == Q.POINTERMOVE) && (this.trackedTouches_[e.pointerId] = e);
		this.activePointers_ = Object.values(this.trackedTouches_);
	}
	handlePointerUp_(e) {
		this.updateActivePointers_(e);
		const t = new Np(Q.POINTERUP, this.map_, e, "undefined", "undefined", this.activePointers_);
		this.dispatchEvent(t);
		if (this.emulateClicks_ && !t.defaultPrevented && !this.dragging_ && this.isMouseActionButton_(e)) {
			this.emulateClick_(this.down_);
		}
		if (this.activePointers_.length === 0) {
			this.dragListenerKeys_.forEach(se);
			this.dragListenerKeys_.length = 0;
			this.dragging_ = false;
			this.down_ = null;
		}
	}
	isMouseActionButton_(e) {
		return e.button === 0;
	}
	handlePointerDown_(e) {
		this.emulateClicks_ = this.activePointers_.length === 0;
		this.updateActivePointers_(e);
		const t = new Np(Q.POINTERDOWN, this.map_, e, "undefined", "undefined", this.activePointers_);
		this.dispatchEvent(t);
		this.down_ = new PointerEvent(e.type, e);
		Object.defineProperty(this.down_, "target", {
			writable: false,
			value: e.target
		});
		if (this.dragListenerKeys_.length === 0) {
			const i = this.map_.getOwnerDocument();
			this.dragListenerKeys_.push(Z(i, Q.POINTERMOVE, this.handlePointerMove_, this), Z(i, Q.POINTERUP, this.handlePointerUp_, this), Z(this.element_, Q.POINTERCANCEL, this.handlePointerUp_, this));
			if (this.element_.getRootNode && this.element_.getRootNode() !== i) {
				this.dragListenerKeys_.push(Z(this.element_.getRootNode(), Q.POINTERUP, this.handlePointerUp_, this));
			}
		}
	}
	handlePointerMove_(e) {
		if (this.isMoving_(e)) {
			this.updateActivePointers_(e);
			this.dragging_ = true;
			const t = new Np(Q.POINTERDRAG, this.map_, e, this.dragging_, "undefined", this.activePointers_);
			this.dispatchEvent(t);
		}
	}
	relayMoveEvent_(e) {
		this.originalPointerMoveEvent_ = e;
		const t = !!(this.down_ && this.isMoving_(e));
		this.dispatchEvent(new Np(Q.POINTERMOVE, this.map_, e, t));
	}
	handleTouchMove_(e) {
		const t = this.originalPointerMoveEvent_;
		if ((!this.originalPointerMoveEvent_ || this.originalPointerMoveEvent_.defaultPrevented) && (typeof e.cancelable != "boolean" || e.cancelable === true)) {
			e.preventDefault();
		}
	}
	isMoving_(e) {
		return this.dragging_ || Math.abs(e.clientX - this.down_.clientX) > this.moveTolerance_ || Math.abs(e.clientY - this.down_.clientY) > this.moveTolerance_;
	}
	disposeInternal() {
		if (this.relayedListenerKey_) {
			se(this.relayedListenerKey_);
			this.relayedListenerKey_ = null;
		}
		this.element_.removeEventListener(U.TOUCHMOVE, this.boundHandleTouchMove_);
		if (this.pointerdownListenerKey_) {
			se(this.pointerdownListenerKey_);
			this.pointerdownListenerKey_ = null;
		}
		this.dragListenerKeys_.forEach(se);
		this.dragListenerKeys_.length = 0;
		this.element_ = null;
		super.disposeInternal();
	}
}
const Zt = {
	POSTRENDER: "postrender",
	MOVESTART: "movestart",
	MOVEEND: "moveend",
	LOADSTART: "loadstart",
	LOADEND: "loadend"
};
const Ge = {
	LAYERGROUP: "layergroup",
	SIZE: "size",
	TARGET: "target",
	VIEW: "view"
};
const Vr = null;
class Bp {
	constructor(e, t) {
		this.priorityFunction_ = e;
		this.keyFunction_ = t;
		this.elements_ = [];
		this.priorities_ = [];
		this.queuedElements_ = {};
	}
	clear() {
		this.elements_.length = 0;
		this.priorities_.length = 0;
		Wi(this.queuedElements_);
	}
	dequeue() {
		const e = this.elements_, t = this.priorities_, i = this.elements_[0];
		this.elements_.length == 1 ? (this.elements_.length = 0, this.priorities_.length = 0) : (this.elements_[0] = this.elements_.pop(), this.priorities_[0] = this.priorities_.pop(), this.siftUp_(0));
		const s = this.keyFunction_(this.elements_[0]);
		delete this.queuedElements_[s];
		return this.elements_[0];
	}
	enqueue(e) {
		ee(!(this.keyFunction_(e) in this.queuedElements_), "Tried to enqueue an `element` that was already added to the queue");
		const t = this.priorityFunction_(e);
		return t != Vr ? (this.elements_.push(e), this.priorities_.push(t), this.queuedElements_[this.keyFunction_(e)] = true, this.siftDown_(0, this.elements_.length - 1), true) : false;
	}
	getCount() {
		return this.elements_.length;
	}
	getLeftChildIndex_(e) {
		return e * 2 + 1;
	}
	getRightChildIndex_(e) {
		return e * 2 + 2;
	}
	getParentIndex_(e) {
		return e - 1 >> 1;
	}
	heapify_() {
		let e;
		for (e = (this.elements_.length >> 1) - 1; e >= 0; e--) this.siftUp_(e);
	}
	isEmpty() {
		return this.elements_.length === 0;
	}
	isKeyQueued(e) {
		return e in this.queuedElements_;
	}
	isQueued(e) {
		return this.isKeyQueued(this.keyFunction_(e));
	}
	siftUp_(e) {
		const t = this.elements_, i = this.priorities_, s = this.elements_.length, r = this.elements_[e], o = this.priorities_[e], a = e;
		for (; e < this.elements_.length >> 1;) {
			const l = this.getLeftChildIndex_(e);
			const h = this.getRightChildIndex_(e);
			const c = h < s && i[h] < i[l] ? h : l;
			t[e] = t[c];
			i[e] = i[c];
			e = c;
		}
		this.elements_[e] = this.elements_[e];
		this.priorities_[e] = this.priorities_[e];
		this.siftDown_(e, e);
	}
	siftDown_(e, t) {
		const i = this.elements_, s = this.priorities_, r = this.elements_[t], o = this.priorities_[t];
		for (; t > e;) {
			const a = this.getParentIndex_(t);
			if (s[a] > o) {
				i[t] = i[a];
				s[t] = s[a];
				t = a;
			} else break;
		}
		this.elements_[t] = this.elements_[t];
		this.priorities_[t] = this.priorities_[t];
	}
	reprioritize() {
		const e = this.priorityFunction_, t = this.elements_, i = this.priorities_;
		let s = 0;
		const r = this.elements_.length;
		let o, a, l;
		for (a = 0; a < this.elements_.length; ++a) {
			o = this.elements_[a];
			l = this.priorityFunction_(o);
			l == Vr ? delete this.queuedElements_[this.keyFunction_(o)] : (this.priorities_[s] = l, this.elements_[s++] = o);
		}
		this.elements_.length = s;
		this.priorities_.length = s;
		this.heapify_();
	}
}
class Up extends Bp {
	constructor(e, t) {
		super((i) => e(), (i) => i[0].getKey());
		this.boundHandleTileChange_ = this.handleTileChange.bind(this);
		this.tileChangeCallback_ = t;
		this.tilesLoading_ = 0;
		this.tilesLoadingKeys_ = {};
	}
	enqueue(e) {
		const t = super.enqueue(e);
		if (t) {
			e[0].addEventListener(U.CHANGE, this.boundHandleTileChange_);
		}
		return t;
	}
	getTilesLoading() {
		return this.tilesLoading_;
	}
	handleTileChange(e) {
		const t = e.target, i = e.target.getState();
		if (i === M.LOADED || i === M.ERROR || i === M.EMPTY) {
			if (i !== M.ERROR) {
				t.removeEventListener(U.CHANGE, this.boundHandleTileChange_);
			}
			const s = t.getKey();
			if (s in this.tilesLoadingKeys_) {
				delete this.tilesLoadingKeys_[s];
				--this.tilesLoading_;
			}
			this.tileChangeCallback_();
		}
	}
	loadMoreTiles(e, t) {
		let i = 0;
		for (; this.tilesLoading_ < e && i < t && this.getCount() > 0;) {
			const s = this.dequeue()[0];
			const r = this.dequeue()[0].getKey();
			if (this.dequeue()[0].getState() === M.IDLE && !(r in this.tilesLoadingKeys_)) {
				this.tilesLoadingKeys_[r] = true;
				++this.tilesLoading_;
				++i;
				this.dequeue()[0].load();
			}
		}
	}
}
function zp(n, e, t, i, s) {
	if (!n || !(t in n.wantedTiles) || !n.wantedTiles[t][e.getKey()]) return Vr;
	const r = n.viewState.center, o = i[0] - n.viewState.center[0], a = i[1] - n.viewState.center[1];
	return 65536 * Math.log(s) + Math.sqrt(o * o + a * a) / s;
}
class Xp extends Lf {
	constructor(e) {
		super();
		const t = e.element;
		if (e.element && !e.target && !e.element.style.pointerEvents) {
			e.element.style.pointerEvents = "auto";
		}
		this.element = e.element || null;
		this.target_ = null;
		this.map_ = null;
		this.listenerKeys = [];
		if (e.render) {
			this.render = e.render;
		}
		if (e.target) {
			this.setTarget(e.target);
		}
	}
	disposeInternal() {
		var e;
		if (!((e = this.element) == null)) {
			e.remove();
		}
		super.disposeInternal();
	}
	getMap() {
		return this.map_;
	}
	setMap(e) {
		var t, i;
		if (this.map_) {
			if (!((t = this.element) == null)) {
				t.remove();
			}
		}
		for (let s = 0, r = this.listenerKeys.length; s < r; ++s) se(this.listenerKeys[s]);
		this.listenerKeys.length = 0;
		this.map_ = e;
		if (e) {
			const s = (i = this.target_) != null ? i : e.getOverlayContainerStopEvent();
			if (this.element) {
				s.appendChild(this.element);
			}
			if (this.render !== fs) {
				this.listenerKeys.push(Z(e, Zt.POSTRENDER, this.render, this));
			}
			e.render();
		}
	}
	render(e) {}
	setTarget(e) {
		this.target_ = typeof e == "string" ? document.getElementById(e) : e;
	}
}
class Wp extends Xp {
	constructor(e) {
		e = e || {};
		super({
			element: document.createElement("div"),
			render: e.render,
			target: e.target
		});
		this.ulElement_ = document.createElement("ul");
		this.collapsed_ = e.collapsed !== "undefined" ? e.collapsed : true;
		this.userCollapsed_ = this.collapsed_;
		this.overrideCollapsible_ = e.collapsible !== "undefined";
		this.collapsible_ = e.collapsible !== "undefined" ? e.collapsible : true;
		if (!this.collapsible_) {
			this.collapsed_ = false;
		}
		this.attributions_ = e.attributions;
		const t = e.className !== "undefined" ? e.className : "ol-attribution", i = e.tipLabel !== "undefined" ? e.tipLabel : "Attributions", s = e.expandClassName !== "undefined" ? e.expandClassName : t + "-expand", r = e.collapseLabel !== "undefined" ? e.collapseLabel : "›", o = e.collapseClassName !== "undefined" ? e.collapseClassName : t + "-collapse";
		typeof r == "string" ? (this.collapseLabel_ = document.createElement("span"), this.collapseLabel_.textContent = r, this.collapseLabel_.className = o) : this.collapseLabel_ = r;
		const a = e.label !== "undefined" ? e.label : "i";
		typeof a == "string" ? (this.label_ = document.createElement("span"), this.label_.textContent = a, this.label_.className = s) : this.label_ = a;
		const l = this.collapsible_ && !this.collapsed_ ? this.collapseLabel_ : this.label_;
		this.toggleButton_ = document.createElement("button");
		this.toggleButton_.setAttribute("type", "button");
		this.toggleButton_.setAttribute("aria-expanded", String(!this.collapsed_));
		this.toggleButton_.title = i;
		this.toggleButton_.appendChild(l);
		this.toggleButton_.addEventListener(U.CLICK, this.handleClick_.bind(this), false);
		const h = t + " " + fo + " " + wl + (this.collapsed_ && this.collapsible_ ? " " + ec : "") + (this.collapsible_ ? "" : " ol-uncollapsible"), c = this.element;
		this.element.className = h;
		this.element.appendChild(this.toggleButton_);
		this.element.appendChild(this.ulElement_);
		this.renderedAttributions_ = [];
		this.renderedVisible_ = true;
	}
	collectSourceAttributions_(e) {
		const t = this.getMap().getAllLayers(), i = new Set(t.flatMap((s) => s.getAttributions(e)));
		if (this.attributions_ !== "undefined") {
			Array.isArray(this.attributions_) ? this.attributions_.forEach((s) => i.add(s)) : i.add(this.attributions_);
		}
		if (!this.overrideCollapsible_) {
			const s = !t.some((r) => {
				var o;
				return ((o = r.getSource()) == null ? "undefined" : o.getAttributionsCollapsible()) === false;
			});
			this.setCollapsible(s);
		}
		return Array.from(i);
	}
	async updateElement_(e) {
		if (!e) {
			if (this.renderedVisible_) {
				this.element.style.display = "none";
				this.renderedVisible_ = false;
			}
			return;
		}
		const t = await Promise.all(this.collectSourceAttributions_(e).map((s) => Zc(() => s))), i = t.length > 0;
		if (this.renderedVisible_ != i) {
			this.element.style.display = i ? "" : "none";
			this.renderedVisible_ = i;
		}
		if (!kt(t, this.renderedAttributions_)) {
			bu(this.ulElement_);
			for (let s = 0, r = t.length; s < r; ++s) {
				const o = document.createElement("li");
				o.innerHTML = t[s];
				this.ulElement_.appendChild(o);
			}
			this.renderedAttributions_ = t;
		}
	}
	handleClick_(e) {
		e.preventDefault();
		this.handleToggle_();
		this.userCollapsed_ = this.collapsed_;
	}
	handleToggle_() {
		this.element.classList.toggle(ec);
		this.collapsed_ ? Jh(this.collapseLabel_, this.label_) : Jh(this.label_, this.collapseLabel_);
		this.collapsed_ = !this.collapsed_;
		this.toggleButton_.setAttribute("aria-expanded", String(!this.collapsed_));
	}
	getCollapsible() {
		return this.collapsible_;
	}
	setCollapsible(e) {
		if (this.collapsible_ !== e) {
			this.collapsible_ = e;
			this.element.classList.toggle("ol-uncollapsible");
			if (this.userCollapsed_) {
				this.handleToggle_();
			}
		}
	}
	setCollapsed(e) {
		this.userCollapsed_ = e;
		if (!(!this.collapsible_ || this.collapsed_ === e)) {
			this.handleToggle_();
		}
	}
	getCollapsed() {
		return this.collapsed_;
	}
	render(e) {
		this.updateElement_(e.frameState);
	}
}
class Yp extends Xp {
	constructor(e) {
		e = e || {};
		super({
			element: document.createElement("div"),
			render: e.render,
			target: e.target
		});
		const t = e.className !== "undefined" ? e.className : "ol-rotate", i = e.label !== "undefined" ? e.label : "⇧", s = e.compassClassName !== "undefined" ? e.compassClassName : "ol-compass";
		this.label_ = null;
		typeof i == "string" ? (this.label_ = document.createElement("span"), this.label_.className = s, this.label_.textContent = i) : (this.label_ = i, this.label_.classList.add(s));
		const r = e.tipLabel ? e.tipLabel : "Reset rotation", o = document.createElement("button");
		o.className = t + "-reset";
		o.setAttribute("type", "button");
		o.title = r;
		o.appendChild(this.label_);
		o.addEventListener(U.CLICK, this.handleClick_.bind(this), false);
		const a = t + " " + fo + " " + wl, l = this.element;
		this.element.className = a;
		this.element.appendChild(o);
		this.callResetNorth_ = e.resetNorth ? e.resetNorth : "undefined";
		this.duration_ = e.duration !== "undefined" ? e.duration : 250;
		this.autoHide_ = e.autoHide !== "undefined" ? e.autoHide : true;
		this.rotation_ = "undefined";
		if (this.autoHide_) {
			this.element.classList.add(tr);
		}
	}
	handleClick_(e) {
		e.preventDefault();
		this.callResetNorth_ !== "undefined" ? this.callResetNorth_() : this.resetNorth_();
	}
	resetNorth_() {
		const t = this.getMap().getView();
		if (!t) return;
		const i = t.getRotation();
		if (i !== "undefined") {
			this.duration_ > 0 && i % (2 * Math.PI) !== 0 ? t.animate({
				rotation: 0,
				duration: this.duration_,
				easing: $n
			}) : t.setRotation(0);
		}
	}
	render(e) {
		const t = e.frameState;
		if (!e.frameState) return;
		const i = e.frameState.viewState.rotation;
		if (e.frameState.viewState.rotation != this.rotation_) {
			const s = "rotate(" + i + "rad)";
			if (this.autoHide_) {
				const r = this.element.classList.contains(tr);
				!r && i === 0 ? this.element.classList.add(tr) : r && i !== 0 && this.element.classList.remove(tr);
			}
			this.label_.style.transform = s;
		}
		this.rotation_ = e.frameState.viewState.rotation;
	}
}
class Kp extends Xp {
	constructor(e) {
		e = e || {};
		super({
			element: document.createElement("div"),
			target: e.target
		});
		const t = e.className !== "undefined" ? e.className : "ol-zoom", i = e.delta !== "undefined" ? e.delta : 1, s = e.zoomInClassName !== "undefined" ? e.zoomInClassName : t + "-in", r = e.zoomOutClassName !== "undefined" ? e.zoomOutClassName : t + "-out", o = e.zoomInLabel !== "undefined" ? e.zoomInLabel : "+", a = e.zoomOutLabel !== "undefined" ? e.zoomOutLabel : "–", l = e.zoomInTipLabel !== "undefined" ? e.zoomInTipLabel : "Zoom in", h = e.zoomOutTipLabel !== "undefined" ? e.zoomOutTipLabel : "Zoom out", c = document.createElement("button");
		c.className = s;
		c.setAttribute("type", "button");
		c.title = l;
		c.appendChild(typeof o == "string" ? document.createTextNode(o) : o);
		c.addEventListener(U.CLICK, this.handleClick_.bind(this, i), false);
		const u = document.createElement("button");
		u.className = r;
		u.setAttribute("type", "button");
		u.title = h;
		u.appendChild(typeof a == "string" ? document.createTextNode(a) : a);
		u.addEventListener(U.CLICK, this.handleClick_.bind(this, -i), false);
		const d = t + " " + fo + " " + wl, f = this.element;
		this.element.className = d;
		this.element.appendChild(c);
		this.element.appendChild(u);
		this.duration_ = e.duration !== "undefined" ? e.duration : 250;
	}
	handleClick_(e, t) {
		t.preventDefault();
		this.zoomByDelta_(e);
	}
	zoomByDelta_(e) {
		const i = this.getMap().getView();
		if (!i) return;
		const s = i.getZoom();
		if (s !== "undefined") {
			const r = i.getConstrainedZoom(s + e);
			this.duration_ > 0 ? (i.getAnimating() && i.cancelAnimations(), i.animate({
				zoom: r,
				duration: this.duration_,
				easing: $n
			})) : i.setZoom(r);
		}
	}
}
function qp(n) {
	n = n || {};
	const e = new Af();
	if (n.zoom !== "undefined" ? n.zoom : true) {
		e.push(new Kp(n.zoomOptions));
	}
	if (n.rotate !== "undefined" ? n.rotate : true) {
		e.push(new Yp(n.rotateOptions));
	}
	if (n.attribution !== "undefined" ? n.attribution : true) {
		e.push(new Wp(n.attributionOptions));
	}
	return e;
}
const Da = { ACTIVE: "active" };
class Jp extends Lf {
	constructor(e) {
		super();
		this.on;
		this.once;
		this.un;
		if (e && e.handleEvent) {
			this.handleEvent = e.handleEvent;
		}
		this.map_ = null;
		this.setActive(true);
	}
	getActive() {
		return this.get(Da.ACTIVE);
	}
	getMap() {
		return this.map_;
	}
	handleEvent(e) {
		return true;
	}
	setActive(e) {
		this.set(Da.ACTIVE, e);
	}
	setMap(e) {
		this.map_ = e;
	}
}
function Qp(n, e, t) {
	const i = n.getCenterInternal();
	if (i) {
		const s = [i[0] + e[0], i[1] + e[1]];
		n.animateInternal({
			duration: t !== "undefined" ? t : 250,
			easing: _p,
			center: n.getConstrainedCenter(s)
		});
	}
}
function Kl(n, e, t, i) {
	const s = n.getZoom();
	if (s === "undefined") return;
	const r = n.getConstrainedZoom(s + e), o = n.getResolutionForZoom(r);
	if (n.getAnimating()) {
		n.cancelAnimations();
	}
	n.animate({
		resolution: o,
		anchor: t,
		duration: i !== "undefined" ? i : 250,
		easing: $n
	});
}
class ey extends Jp {
	constructor(e) {
		super();
		e = e || {};
		this.delta_ = e.delta ? e.delta : 1;
		this.duration_ = e.duration !== "undefined" ? e.duration : 250;
	}
	handleEvent(e) {
		let t = false;
		if (e.type == Q.DBLCLICK) {
			const i = e.originalEvent;
			const s = e.map;
			const r = e.coordinate;
			const o = e.originalEvent.shiftKey ? -this.delta_ : this.delta_;
			const a = e.map.getView();
			Kl(a, o, e.coordinate, this.duration_);
			e.originalEvent.preventDefault();
			t = true;
		}
		return !t;
	}
}
function Na(n) {
	const e = arguments;
	return function(t) {
		let i = true;
		for (let s = 0, r = e.length; s < r && (i = i && e[s](t), !!i); ++s);
		return i;
	};
}
const iy = function(n) {
	const e = n.originalEvent;
	return n.originalEvent.altKey && !(n.originalEvent.metaKey || n.originalEvent.ctrlKey) && !n.originalEvent.shiftKey;
};
const ny = function(n) {
	const e = n.originalEvent;
	return n.originalEvent.altKey && !(n.originalEvent.metaKey || n.originalEvent.ctrlKey) && n.originalEvent.shiftKey;
};
const sy = function(n) {
	const e = n.map.getTargetElement(), t = e.getRootNode(), i = n.map.getOwnerDocument().activeElement;
	return t instanceof ShadowRoot ? t.host.contains(n.map.getOwnerDocument().activeElement) : e.contains(n.map.getOwnerDocument().activeElement);
};
const fd = function(n) {
	const e = n.map.getTargetElement(), t = e.getRootNode();
	return (t instanceof ShadowRoot ? t.host : e).hasAttribute("tabindex") ? sy(n) : true;
};
const gd = function(n) {
	const e = n.originalEvent;
	return "pointerId" in n.originalEvent && n.originalEvent.button == 0 && !(u_ && El && n.originalEvent.ctrlKey);
};
const _d = function(n) {
	return n.type == Q.SINGLECLICK;
};
const Hl = function(n) {
	const e = n.originalEvent;
	return !n.originalEvent.altKey && !(n.originalEvent.metaKey || n.originalEvent.ctrlKey) && !n.originalEvent.shiftKey;
};
const UC = function(n) {
	const e = n.originalEvent;
	return !n.originalEvent.altKey && (El ? n.originalEvent.metaKey : n.originalEvent.ctrlKey) && !n.originalEvent.shiftKey;
};
const ry = function(n) {
	const e = n.originalEvent;
	return El ? n.originalEvent.metaKey : n.originalEvent.ctrlKey;
};
const Zr = function(n) {
	const e = n.originalEvent;
	return !n.originalEvent.altKey && !(n.originalEvent.metaKey || n.originalEvent.ctrlKey) && n.originalEvent.shiftKey;
};
const md = function(n) {
	const e = n.originalEvent, t = n.originalEvent.target.tagName;
	return n.originalEvent.target.tagName !== "INPUT" && n.originalEvent.target.tagName !== "SELECT" && n.originalEvent.target.tagName !== "TEXTAREA" && !n.originalEvent.target.isContentEditable;
};
const Ho = function(n) {
	const e = n.originalEvent;
	return "pointerId" in n.originalEvent && n.originalEvent.pointerType == "mouse";
};
const pd = function(n) {
	const e = n.originalEvent;
	return "pointerId" in n.originalEvent && n.originalEvent.isPrimary && n.originalEvent.button === 0;
};
class oy extends Jp {
	constructor(e) {
		e = e || {};
		super(e);
		if (e.handleDownEvent) {
			this.handleDownEvent = e.handleDownEvent;
		}
		if (e.handleDragEvent) {
			this.handleDragEvent = e.handleDragEvent;
		}
		if (e.handleMoveEvent) {
			this.handleMoveEvent = e.handleMoveEvent;
		}
		if (e.handleUpEvent) {
			this.handleUpEvent = e.handleUpEvent;
		}
		if (e.stopDown) {
			this.stopDown = e.stopDown;
		}
		this.handlingDownUpSequence = false;
		this.targetPointers = [];
	}
	getPointerCount() {
		return this.targetPointers.length;
	}
	handleDownEvent(e) {
		return false;
	}
	handleDragEvent(e) {}
	handleEvent(e) {
		if (!e.originalEvent) return true;
		let t = false;
		this.updateTrackedPointers_(e);
		if (this.handlingDownUpSequence) {
			if (e.type == Q.POINTERDRAG) {
				this.handleDragEvent(e);
				e.originalEvent.preventDefault();
			} else if (e.type == Q.POINTERUP) {
				const i = this.handleUpEvent(e);
				this.handlingDownUpSequence = i && this.targetPointers.length > 0;
			}
		} else if (e.type == Q.POINTERDOWN) {
			const i = this.handleDownEvent(e);
			this.handlingDownUpSequence = i;
			t = this.stopDown(i);
		} else e.type == Q.POINTERMOVE && this.handleMoveEvent(e);
		return !t;
	}
	handleMoveEvent(e) {}
	handleUpEvent(e) {
		return false;
	}
	stopDown(e) {
		return e;
	}
	updateTrackedPointers_(e) {
		if (e.activePointers) {
			this.targetPointers = e.activePointers;
		}
	}
}
function ql(n) {
	const e = n.length;
	let t = 0, i = 0;
	for (let s = 0; s < n.length; s++) {
		t += n[s].clientX;
		i += n[s].clientY;
	}
	return {
		clientX: t / n.length,
		clientY: i / n.length
	};
}
class ay extends oy {
	constructor(e) {
		super({ stopDown: Xi });
		e = e || {};
		this.kinetic_ = e.kinetic;
		this.lastCentroid = null;
		this.lastPointersCount_;
		this.panning_ = false;
		const t = e.condition ? e.condition : Na(Hl, pd);
		this.condition_ = e.onFocusOnly ? Na(fd, t) : t;
		this.noKinetic_ = false;
	}
	handleDragEvent(e) {
		const t = e.map;
		if (!this.panning_) {
			this.panning_ = true;
			e.map.getView().beginInteraction();
		}
		const i = this.targetPointers, s = e.map.getEventPixel(ql(this.targetPointers));
		if (this.targetPointers.length == this.lastPointersCount_) {
			if (this.kinetic_) {
				this.kinetic_.update(s[0], s[1]);
			}
			if (this.lastCentroid) {
				const r = [this.lastCentroid[0] - s[0], s[1] - this.lastCentroid[1]];
				const a = e.map.getView();
				Xf(r, a.getResolution());
				qa(r, a.getRotation());
				a.adjustCenterInternal(r);
			}
		} else this.kinetic_ && this.kinetic_.begin();
		this.lastCentroid = s;
		this.lastPointersCount_ = this.targetPointers.length;
		e.originalEvent.preventDefault();
	}
	handleUpEvent(e) {
		const t = e.map, i = e.map.getView();
		if (this.targetPointers.length === 0) {
			if (!this.noKinetic_ && this.kinetic_ && this.kinetic_.end()) {
				const s = this.kinetic_.getDistance();
				const r = this.kinetic_.getAngle();
				const o = i.getCenterInternal();
				const a = t.getPixelFromCoordinateInternal(o);
				const l = t.getCoordinateFromPixelInternal([a[0] - s * Math.cos(r), a[1] - s * Math.sin(r)]);
				i.animateInternal({
					center: i.getConstrainedCenter(l),
					duration: 500,
					easing: $n
				});
			}
			if (this.panning_) {
				this.panning_ = false;
				i.endInteraction();
			}
			return false;
		}
		if (this.kinetic_) {
			this.kinetic_.begin();
		}
		this.lastCentroid = null;
		return true;
	}
	handleDownEvent(e) {
		if (this.targetPointers.length > 0 && this.condition_(e)) {
			const i = e.map.getView();
			this.lastCentroid = null;
			if (i.getAnimating()) {
				i.cancelAnimations();
			}
			if (this.kinetic_) {
				this.kinetic_.begin();
			}
			this.noKinetic_ = this.targetPointers.length > 1;
			return true;
		}
		return false;
	}
}
class hy extends oy {
	constructor(e) {
		e = e || {};
		super({ stopDown: Xi });
		this.condition_ = e.condition ? e.condition : ny;
		this.lastAngle_ = "undefined";
		this.duration_ = e.duration !== "undefined" ? e.duration : 250;
	}
	handleDragEvent(e) {
		if (!Ho(e)) return;
		const t = e.map, i = e.map.getView();
		if (i.getConstraints().rotation === Xl) return;
		const s = e.map.getSize(), r = e.pixel, o = Math.atan2(s[1] / 2 - e.pixel[1], e.pixel[0] - s[0] / 2);
		if (this.lastAngle_ !== "undefined") {
			const a = o - this.lastAngle_;
			i.adjustRotationInternal(-a);
		}
		this.lastAngle_ = o;
	}
	handleUpEvent(e) {
		return Ho(e) ? (e.map.getView().endInteraction(this.duration_), false) : true;
	}
	handleDownEvent(e) {
		return Ho(e) && gd(e) && this.condition_(e) ? (e.map.getView().beginInteraction(), this.lastAngle_ = "undefined", true) : false;
	}
}
class uy extends Tf {
	constructor(e) {
		super();
		this.geometry_ = null;
		this.element_ = document.createElement("div");
		this.element_.style.position = "absolute";
		this.element_.style.pointerEvents = "auto";
		this.element_.className = "ol-box " + e;
		this.map_ = null;
		this.startPixel_ = null;
		this.endPixel_ = null;
	}
	disposeInternal() {
		this.setMap(null);
	}
	render_() {
		const e = this.startPixel_, t = this.endPixel_, i = "px", s = this.element_.style;
		this.element_.style.left = Math.min(this.startPixel_[0], this.endPixel_[0]) + "px";
		this.element_.style.top = Math.min(this.startPixel_[1], this.endPixel_[1]) + "px";
		this.element_.style.width = Math.abs(this.endPixel_[0] - this.startPixel_[0]) + "px";
		this.element_.style.height = Math.abs(this.endPixel_[1] - this.startPixel_[1]) + "px";
	}
	setMap(e) {
		if (this.map_) {
			this.map_.getOverlayContainer().removeChild(this.element_);
			const t = this.element_.style;
			this.element_.style.left = "inherit";
			this.element_.style.top = "inherit";
			this.element_.style.width = "inherit";
			this.element_.style.height = "inherit";
		}
		this.map_ = e;
		if (this.map_) {
			this.map_.getOverlayContainer().appendChild(this.element_);
		}
	}
	setPixels(e, t) {
		this.startPixel_ = e;
		this.endPixel_ = t;
		this.createOrUpdateGeometry();
		this.render_();
	}
	createOrUpdateGeometry() {
		if (!this.map_) return;
		const e = this.startPixel_, t = this.endPixel_, s = [
			this.startPixel_,
			[this.startPixel_[0], this.endPixel_[1]],
			this.endPixel_,
			[this.endPixel_[0], this.startPixel_[1]]
		].map(this.map_.getCoordinateFromPixelInternal, this.map_);
		s[4] = s[0].slice();
		this.geometry_ ? this.geometry_.setCoordinates([s]) : this.geometry_ = new wn([s]);
	}
	getGeometry() {
		return this.geometry_;
	}
}
const tn = {
	BOXSTART: "boxstart",
	BOXDRAG: "boxdrag",
	BOXEND: "boxend",
	BOXCANCEL: "boxcancel"
};
class Qn extends wf {
	constructor(e, t, i) {
		super(e);
		this.coordinate = t;
		this.mapBrowserEvent = i;
	}
}
class fy extends oy {
	constructor(e) {
		var t, i, s;
		super();
		this.on;
		this.once;
		this.un;
		e = e != null ? e : {};
		this.box_ = new uy(e.className || "ol-dragbox");
		this.minArea_ = (t = e.minArea) != null ? t : 64;
		if (e.onBoxEnd) {
			this.onBoxEnd = e.onBoxEnd;
		}
		this.startPixel_ = null;
		this.condition_ = (i = e.condition) != null ? i : gd;
		this.boxEndCondition_ = (s = e.boxEndCondition) != null ? s : this.defaultBoxEndCondition;
	}
	defaultBoxEndCondition(e, t, i) {
		const s = i[0] - t[0], r = i[1] - t[1];
		return s * s + r * r >= this.minArea_;
	}
	getGeometry() {
		return this.box_.getGeometry();
	}
	handleDragEvent(e) {
		if (this.startPixel_) {
			this.box_.setPixels(this.startPixel_, e.pixel);
			this.dispatchEvent(new Qn(tn.BOXDRAG, e.coordinate, e));
		}
	}
	handleUpEvent(e) {
		if (!this.startPixel_) return false;
		const t = this.boxEndCondition_(e, this.startPixel_, e.pixel);
		if (t) {
			this.onBoxEnd(e);
		}
		this.dispatchEvent(new Qn(t ? tn.BOXEND : tn.BOXCANCEL, e.coordinate, e));
		this.box_.setMap(null);
		this.startPixel_ = null;
		return false;
	}
	handleDownEvent(e) {
		return this.condition_(e) ? (this.startPixel_ = e.pixel, this.box_.setMap(e.map), this.box_.setPixels(this.startPixel_, this.startPixel_), this.dispatchEvent(new Qn(tn.BOXSTART, e.coordinate, e)), true) : false;
	}
	onBoxEnd(e) {}
	setActive(e) {
		if (!e) {
			this.box_.setMap(null);
			if (this.startPixel_) {
				this.dispatchEvent(new Qn(tn.BOXCANCEL, this.startPixel_, null));
				this.startPixel_ = null;
			}
		}
		super.setActive(e);
	}
	setMap(e) {
		if (this.getMap()) {
			this.box_.setMap(null);
			if (this.startPixel_) {
				this.dispatchEvent(new Qn(tn.BOXCANCEL, this.startPixel_, null));
				this.startPixel_ = null;
			}
		}
		super.setMap(e);
	}
}
class _y extends fy {
	constructor(e) {
		e = e || {};
		const t = e.condition ? e.condition : Zr;
		super({
			condition: t,
			className: e.className || "ol-dragzoom",
			minArea: e.minArea
		});
		this.duration_ = e.duration !== "undefined" ? e.duration : 200;
		this.out_ = e.out !== "undefined" ? e.out : false;
	}
	onBoxEnd(e) {
		const i = this.getMap().getView();
		let s = this.getGeometry();
		if (this.out_) {
			const r = i.rotatedExtentForGeometry(s);
			const o = i.getResolutionForExtentInternal(r);
			const a = i.getResolution() / o;
			s = s.clone();
			s.scale(a * a);
		}
		i.fitInternal(s, {
			duration: this.duration_,
			easing: $n
		});
	}
}
const Oi = {
	LEFT: "ArrowLeft",
	UP: "ArrowUp",
	RIGHT: "ArrowRight",
	DOWN: "ArrowDown"
};
class py extends Jp {
	constructor(e) {
		super();
		e = e || {};
		this.defaultCondition_ = function(t) {
			return Hl(t) && md(t);
		};
		this.condition_ = e.condition !== "undefined" ? e.condition : this.defaultCondition_;
		this.duration_ = e.duration !== "undefined" ? e.duration : 100;
		this.pixelDelta_ = e.pixelDelta !== "undefined" ? e.pixelDelta : 128;
	}
	handleEvent(e) {
		let t = false;
		if (e.type == U.KEYDOWN) {
			const i = e.originalEvent;
			const s = e.originalEvent.key;
			if (this.condition_(e) && (e.originalEvent.key == Oi.DOWN || e.originalEvent.key == Oi.LEFT || e.originalEvent.key == Oi.RIGHT || e.originalEvent.key == Oi.UP)) {
				const o = e.map.getView();
				const a = o.getResolution() * this.pixelDelta_;
				let l = 0;
				let h = 0;
				s == Oi.DOWN ? h = -a : s == Oi.LEFT ? l = -a : s == Oi.RIGHT ? l = a : h = a;
				const c = [l, h];
				qa(c, o.getRotation());
				Qp(o, c, this.duration_);
				i.preventDefault();
				t = true;
			}
		}
		return !t;
	}
}
class xy extends Jp {
	constructor(e) {
		super();
		e = e || {};
		this.condition_ = e.condition ? e.condition : function(t) {
			return !ry(t) && md(t);
		};
		this.delta_ = e.delta ? e.delta : 1;
		this.duration_ = e.duration !== "undefined" ? e.duration : 100;
	}
	handleEvent(e) {
		let t = false;
		if (e.type == U.KEYDOWN || e.type == U.KEYPRESS) {
			const i = e.originalEvent;
			const s = e.originalEvent.key;
			if (this.condition_(e) && (e.originalEvent.key === "+" || e.originalEvent.key === "-")) {
				const r = e.map;
				const o = s === "+" ? this.delta_ : -this.delta_;
				const a = e.map.getView();
				Kl(a, o, "undefined", this.duration_);
				i.preventDefault();
				t = true;
			}
		}
		return !t;
	}
}
const Ty = 40;
const Cy = 300;
class Ry extends Jp {
	constructor(e) {
		e = e || {};
		super(e);
		this.totalDelta_ = 0;
		this.lastDelta_ = 0;
		this.maxDelta_ = e.maxDelta !== "undefined" ? e.maxDelta : 1;
		this.duration_ = e.duration !== "undefined" ? e.duration : 250;
		this.timeout_ = e.timeout !== "undefined" ? e.timeout : 80;
		this.useAnchor_ = e.useAnchor !== "undefined" ? e.useAnchor : true;
		this.constrainResolution_ = e.constrainResolution !== "undefined" ? e.constrainResolution : false;
		const t = e.condition ? e.condition : ni;
		this.condition_ = e.onFocusOnly ? Na(fd, t) : t;
		this.lastAnchor_ = null;
		this.startTime_ = "undefined";
		this.timeoutId_;
		this.mode_ = "undefined";
		this.trackpadEventGap_ = 400;
		this.trackpadTimeoutId_;
		this.deltaPerZoom_ = 300;
	}
	endInteraction_() {
		this.trackpadTimeoutId_ = "undefined";
		const e = this.getMap();
		if (!e) return;
		e.getView().endInteraction("undefined", this.lastDelta_ ? this.lastDelta_ > 0 ? 1 : -1 : 0, this.lastAnchor_ ? e.getCoordinateFromPixel(this.lastAnchor_) : null);
	}
	handleEvent(e) {
		if (!this.condition_(e) || e.type !== U.WHEEL) return true;
		const i = e.map, s = e.originalEvent;
		e.originalEvent.preventDefault();
		if (this.useAnchor_) {
			this.lastAnchor_ = e.pixel;
		}
		let r = e.originalEvent.deltaY;
		switch (e.originalEvent.deltaMode) {
			case WheelEvent.DOM_DELTA_LINE:
				r *= Ty;
				break;
			case WheelEvent.DOM_DELTA_PAGE:
				r *= Cy;
				break;
		}
		if (r === 0) return false;
		this.lastDelta_ = r;
		const o = Date.now();
		if (this.startTime_ === "undefined") {
			this.startTime_ = o;
		}
		if (!this.mode_ || o - this.startTime_ > this.trackpadEventGap_) {
			this.mode_ = Math.abs(r) < 4 ? "trackpad" : "wheel";
		}
		const a = e.map.getView();
		if (this.mode_ === "trackpad" && !(a.getConstrainResolution() || this.constrainResolution_)) return this.trackpadTimeoutId_ ? clearTimeout(this.trackpadTimeoutId_) : (a.getAnimating() && a.cancelAnimations(), a.beginInteraction()), this.trackpadTimeoutId_ = setTimeout(this.endInteraction_.bind(this), this.timeout_), a.adjustZoom(-r / this.deltaPerZoom_, this.lastAnchor_ ? e.map.getCoordinateFromPixel(this.lastAnchor_) : null), this.startTime_ = o, false;
		this.totalDelta_ += r;
		const l = Math.max(this.timeout_ - (o - this.startTime_), 0);
		clearTimeout(this.timeoutId_);
		this.timeoutId_ = setTimeout(this.handleWheelZoom_.bind(this, e.map), l);
		return false;
	}
	handleWheelZoom_(e) {
		const t = e.getView();
		if (t.getAnimating()) {
			t.cancelAnimations();
		}
		let i = -fe(this.totalDelta_, -this.maxDelta_ * this.deltaPerZoom_, this.maxDelta_ * this.deltaPerZoom_) / this.deltaPerZoom_;
		if (t.getConstrainResolution() || this.constrainResolution_) {
			i = i ? i > 0 ? 1 : -1 : 0;
		}
		Kl(t, i, this.lastAnchor_ ? e.getCoordinateFromPixel(this.lastAnchor_) : null, this.duration_);
		this.mode_ = "undefined";
		this.totalDelta_ = 0;
		this.lastAnchor_ = null;
		this.startTime_ = "undefined";
		this.timeoutId_ = "undefined";
	}
	setMouseAnchor(e) {
		this.useAnchor_ = e;
		if (!e) {
			this.lastAnchor_ = null;
		}
	}
}
class vy extends oy {
	constructor(e) {
		e = e || {};
		const t = e;
		if (!e.stopDown) {
			e.stopDown = Xi;
		}
		super(e);
		this.anchor_ = null;
		this.lastAngle_ = "undefined";
		this.rotating_ = false;
		this.rotationDelta_ = 0;
		this.threshold_ = e.threshold !== "undefined" ? e.threshold : .3;
		this.duration_ = e.duration !== "undefined" ? e.duration : 250;
	}
	handleDragEvent(e) {
		let t = 0;
		const i = this.targetPointers[0], s = this.targetPointers[1], r = Math.atan2(this.targetPointers[1].clientY - this.targetPointers[0].clientY, this.targetPointers[1].clientX - this.targetPointers[0].clientX);
		if (this.lastAngle_ !== "undefined") {
			const l = r - this.lastAngle_;
			this.rotationDelta_ += l;
			if (!this.rotating_ && Math.abs(this.rotationDelta_) > this.threshold_) {
				this.rotating_ = true;
			}
			t = l;
		}
		this.lastAngle_ = r;
		const o = e.map, a = e.map.getView();
		if (a.getConstraints().rotation !== Xl) {
			this.anchor_ = e.map.getCoordinateFromPixelInternal(e.map.getEventPixel(ql(this.targetPointers)));
			if (this.rotating_) {
				e.map.render();
				a.adjustRotationInternal(t, this.anchor_);
			}
		}
	}
	handleUpEvent(e) {
		return this.targetPointers.length < 2 ? (e.map.getView().endInteraction(this.duration_), false) : true;
	}
	handleDownEvent(e) {
		if (this.targetPointers.length >= 2) {
			const t = e.map;
			this.anchor_ = null;
			this.lastAngle_ = "undefined";
			this.rotating_ = false;
			this.rotationDelta_ = 0;
			if (!this.handlingDownUpSequence) {
				e.map.getView().beginInteraction();
			}
			return true;
		}
		return false;
	}
}
class Py extends oy {
	constructor(e) {
		e = e || {};
		const t = e;
		if (!e.stopDown) {
			e.stopDown = Xi;
		}
		super(e);
		this.anchor_ = null;
		this.duration_ = e.duration !== "undefined" ? e.duration : 400;
		this.lastDistance_ = "undefined";
		this.lastScaleDelta_ = 1;
	}
	handleDragEvent(e) {
		let t = 1;
		const i = this.targetPointers[0], s = this.targetPointers[1], r = this.targetPointers[0].clientX - this.targetPointers[1].clientX, o = this.targetPointers[0].clientY - this.targetPointers[1].clientY, a = Math.sqrt(r * r + o * o);
		if (this.lastDistance_ !== "undefined") {
			t = this.lastDistance_ / a;
		}
		this.lastDistance_ = a;
		const l = e.map, h = e.map.getView();
		this.anchor_ = e.map.getCoordinateFromPixelInternal(e.map.getEventPixel(ql(this.targetPointers)));
		e.map.render();
		h.adjustResolutionInternal(t, this.anchor_);
	}
	handleUpEvent(e) {
		if (this.targetPointers.length < 2) {
			const i = e.map.getView();
			const s = this.lastScaleDelta_ > 1 ? 1 : -1;
			i.endInteraction(this.duration_, s);
			return false;
		}
		return true;
	}
	handleDownEvent(e) {
		if (this.targetPointers.length >= 2) {
			const t = e.map;
			this.anchor_ = null;
			this.lastDistance_ = "undefined";
			this.lastScaleDelta_ = 1;
			if (!this.handlingDownUpSequence) {
				e.map.getView().beginInteraction();
			}
			return true;
		}
		return false;
	}
}
function Fy(n) {
	n = n || {};
	const e = new Af(), t = new bp(-.005, .05, 100);
	if (n.altShiftDragRotate !== "undefined" ? n.altShiftDragRotate : true) {
		e.push(new hy());
	}
	if (n.doubleClickZoom !== "undefined" ? n.doubleClickZoom : true) {
		e.push(new ey({
			delta: n.zoomDelta,
			duration: n.zoomDuration
		}));
	}
	if (n.dragPan !== "undefined" ? n.dragPan : true) {
		e.push(new ay({
			onFocusOnly: n.onFocusOnly,
			kinetic: t
		}));
	}
	if (n.pinchRotate !== "undefined" ? n.pinchRotate : true) {
		e.push(new vy());
	}
	if (n.pinchZoom !== "undefined" ? n.pinchZoom : true) {
		e.push(new Py({ duration: n.zoomDuration }));
	}
	if (n.keyboard !== "undefined" ? n.keyboard : true) {
		e.push(new py());
		e.push(new xy({
			delta: n.zoomDelta,
			duration: n.zoomDuration
		}));
	}
	if (n.mouseWheelZoom !== "undefined" ? n.mouseWheelZoom : true) {
		e.push(new Ry({
			onFocusOnly: n.onFocusOnly,
			duration: n.zoomDuration
		}));
	}
	if (n.shiftDragZoom !== "undefined" ? n.shiftDragZoom : true) {
		e.push(new _y({ duration: n.zoomDuration }));
	}
	return e;
}
const di = {
	ADDLAYER: "addlayer",
	REMOVELAYER: "removelayer"
};
class gi extends wf {
	constructor(e, t) {
		super(e);
		this.layer = t;
	}
}
const qo = { LAYERS: "layers" };
class Jl extends vp {
	constructor(e) {
		e = e || {};
		const t = Object.assign({}, e);
		delete t.layers;
		let i = e.layers;
		super(t);
		this.on;
		this.once;
		this.un;
		this.layersListenerKeys_ = [];
		this.listenerKeys_ = {};
		this.addChangeListener(qo.LAYERS, this.handleLayersChanged_);
		i ? Array.isArray(i) ? i = new Af(i.slice(), { unique: true }) : ee(typeof i.getArray == "function", "Expected `layers` to be an array or a `Collection`") : i = new Af("undefined", { unique: true });
		this.setLayers(i);
	}
	handleLayerChange_() {
		this.changed();
	}
	handleLayersChanged_() {
		this.layersListenerKeys_.forEach(se);
		this.layersListenerKeys_.length = 0;
		const e = this.getLayers();
		this.layersListenerKeys_.push(Z(e, _e.ADD, this.handleLayersAdd_, this), Z(e, _e.REMOVE, this.handleLayersRemove_, this));
		for (const i in this.listenerKeys_) this.listenerKeys_[i].forEach(se);
		Wi(this.listenerKeys_);
		const t = e.getArray();
		for (let i = 0, s = t.length; i < s; i++) {
			const r = t[i];
			this.registerLayerListeners_(t[i]);
			this.dispatchEvent(new gi(di.ADDLAYER, t[i]));
		}
		this.changed();
	}
	registerLayerListeners_(e) {
		const t = [Z(e, xt.PROPERTYCHANGE, this.handleLayerChange_, this), Z(e, U.CHANGE, this.handleLayerChange_, this)];
		if (e instanceof Jl) {
			t.push(Z(e, di.ADDLAYER, this.handleLayerGroupAdd_, this), Z(e, di.REMOVELAYER, this.handleLayerGroupRemove_, this));
		}
		this.listenerKeys_[O(e)] = t;
	}
	handleLayerGroupAdd_(e) {
		this.dispatchEvent(new gi(di.ADDLAYER, e.layer));
	}
	handleLayerGroupRemove_(e) {
		this.dispatchEvent(new gi(di.REMOVELAYER, e.layer));
	}
	handleLayersAdd_(e) {
		const t = e.element;
		this.registerLayerListeners_(e.element);
		this.dispatchEvent(new gi(di.ADDLAYER, e.element));
		this.changed();
	}
	handleLayersRemove_(e) {
		const t = e.element, i = O(e.element);
		this.listenerKeys_[i].forEach(se);
		delete this.listenerKeys_[i];
		this.dispatchEvent(new gi(di.REMOVELAYER, e.element));
		this.changed();
	}
	getLayers() {
		return this.get(qo.LAYERS);
	}
	setLayers(e) {
		const t = this.getLayers();
		if (t) {
			const i = t.getArray();
			for (let s = 0, r = i.length; s < r; ++s) this.dispatchEvent(new gi(di.REMOVELAYER, i[s]));
		}
		this.set(qo.LAYERS, e);
	}
	getLayersArray(e) {
		e = e !== "undefined" ? e : [];
		this.getLayers().forEach(function(t) {
			t.getLayersArray(e);
		});
		return e;
	}
	getLayerStatesArray(e) {
		const t = e !== "undefined" ? e : [], i = t.length;
		this.getLayers().forEach(function(o) {
			o.getLayerStatesArray(t);
		});
		const s = this.getLayerState();
		let r = s.zIndex;
		if (!e && s.zIndex === "undefined") {
			r = 0;
		}
		for (let o = t.length, a = t.length; o < a; o++) {
			const l = t[o];
			t[o].opacity *= s.opacity;
			t[o].visible = t[o].visible && s.visible;
			t[o].maxResolution = Math.min(t[o].maxResolution, s.maxResolution);
			t[o].minResolution = Math.max(t[o].minResolution, s.minResolution);
			t[o].minZoom = Math.max(t[o].minZoom, s.minZoom);
			t[o].maxZoom = Math.min(t[o].maxZoom, s.maxZoom);
			if (s.extent !== "undefined") {
				t[o].extent !== "undefined" ? t[o].extent = Et(t[o].extent, s.extent) : t[o].extent = s.extent;
			}
			if (t[o].zIndex === "undefined") {
				t[o].zIndex = r;
			}
		}
		return t;
	}
	getSourceState() {
		return "ready";
	}
}
class Ly extends Tf {
	constructor(e) {
		super();
		this.map_ = e;
	}
	dispatchRenderEvent(e, t) {
		z();
	}
	calculateMatrices2D(e) {
		const t = e.viewState, i = e.coordinateToPixelTransform, s = e.pixelToCoordinateTransform;
		gt(e.coordinateToPixelTransform, e.size[0] / 2, e.size[1] / 2, 1 / e.viewState.resolution, -1 / e.viewState.resolution, -e.viewState.rotation, -e.viewState.center[0], -e.viewState.center[1]);
		ps(e.pixelToCoordinateTransform, e.coordinateToPixelTransform);
	}
	forEachFeatureAtCoordinate(e, t, i, s, r, o, a, l) {
		let h;
		const c = t.viewState;
		function u(x, T, v, P) {
			return r.call(o, T, x ? v : null, P);
		}
		const d = t.viewState.projection, f = Ja(e.slice(), t.viewState.projection), g = [[0, 0]];
		if (t.viewState.projection.canWrapX() && s) {
			const x = d.getExtent();
			const T = J(x);
			g.push([-T, 0], [T, 0]);
		}
		const m = t.layerStatesArray, _ = t.layerStatesArray.length, p = [], y = [];
		for (let x = 0; x < g.length; x++) for (let T = t.layerStatesArray.length - 1; T >= 0; --T) {
			const v = m[T];
			const P = m[T].layer;
			if (m[T].layer.hasRenderer() && Vl(m[T], c) && a.call(l, m[T].layer)) {
				const S = P.getRenderer();
				const R = P.getSource();
				if (S && R) {
					const I = R.getWrapX() ? f : e;
					const N = u.bind(null, v.managed);
					y[0] = I[0] + g[x][0];
					y[1] = I[1] + g[x][1];
					h = S.forEachFeatureAtCoordinate(y, t, i, N, p);
				}
				if (h) return h;
			}
		}
		if (p.length === 0) return;
		const E = 1 / p.length;
		p.forEach((x, T) => x.distanceSq += T * E);
		p.sort((x, T) => x.distanceSq - T.distanceSq);
		p.some((x) => h = x.callback(x.feature, x.layer, x.geometry));
		return h;
	}
	hasFeatureAtCoordinate(e, t, i, s, r, o) {
		return this.forEachFeatureAtCoordinate(e, t, i, s, ni, this, r, o) !== "undefined";
	}
	getMap() {
		return this.map_;
	}
	renderFrame(e) {
		z();
	}
	scheduleExpireIconCache(e) {
		if (ct.canExpireCache()) {
			e.postRenderFunctions.push(Ay);
		}
	}
}
function Ay(n, e) {
	ct.expire();
}
class by extends Ly {
	constructor(e) {
		super(e);
		this.fontChangeListenerKey_ = Z(un, xt.PROPERTYCHANGE, e.redrawText, e);
		this.element_ = ht ? Ou() : document.createElement("div");
		const t = this.element_.style;
		this.element_.style.position = "absolute";
		this.element_.style.width = "100%";
		this.element_.style.height = "100%";
		this.element_.style.zIndex = "0";
		this.element_.className = fo + " ol-layers";
		const i = e.getViewport();
		if (i) {
			i.insertBefore(this.element_, i.firstChild || null);
		}
		this.children_ = [];
		this.renderedVisible_ = true;
	}
	dispatchRenderEvent(e, t) {
		const i = this.getMap();
		if (i.hasListener(e)) {
			const s = new Sm(e, "undefined", t);
			i.dispatchEvent(s);
		}
	}
	disposeInternal() {
		se(this.fontChangeListenerKey_);
		this.element_.remove();
		super.disposeInternal();
	}
	renderFrame(e) {
		if (!e) {
			if (this.renderedVisible_) {
				this.element_.style.display = "none";
				this.renderedVisible_ = false;
			}
			return;
		}
		this.calculateMatrices2D(e);
		this.dispatchRenderEvent(Me.PRECOMPOSE, e);
		const t = e.layerStatesArray.sort((h, c) => h.zIndex - c.zIndex);
		if (t.some((h) => )) {
			e.declutter = {};
		}
		const s = e.viewState;
		this.children_.length = 0;
		const r = [];
		let o = null;
		for (let h = 0, c = t.length; h < c; ++h) {
			const u = t[h];
			e.layerIndex = h;
			const d = t[h].layer;
			const f = t[h].layer.getSourceState();
			if (!Vl(t[h], s) || f != "ready" && f != "undefined") {
				d.unrender();
				continue;
			}
			const g = t[h].layer.render(e, o);
			if (g) {
				if (g !== o) {
					this.children_.push(g);
					o = g;
				}
				r.push(t[h]);
			}
		}
		this.declutter(e, r);
		g_(this.element_, this.children_);
		const l = this.getMap().getTargetElement();
		if (pi(l)) {
			const h = l.getContext("2d");
			for (const c of this.children_) {
				const u = c.firstElementChild || c;
				const d = c.style.backgroundColor;
				if (c.style.backgroundColor && (!pi(u) || u.width > 0)) {
					h.fillStyle = c.style.backgroundColor;
					h.fillRect(0, 0, l.width, l.height);
				}
				if (pi(u) && u.width > 0) {
					h.save();
					const f = c.style.opacity || u.style.opacity;
					h.globalAlpha = f === "" ? 1 : Number(f);
					const g = u.style.transform;
					if (u.style.transform) h.transform(...wa(u.style.transform));
					else {
						const m = parseFloat(u.style.width) / u.width;
						const _ = parseFloat(u.style.height) / u.height;
						h.transform(m, 0, 0, _, 0, 0);
					}
					h.drawImage(u, 0, 0);
					h.restore();
				}
			}
		}
		this.dispatchRenderEvent(Me.POSTCOMPOSE, e);
		if (!this.renderedVisible_) {
			this.element_.style.display = "";
			this.renderedVisible_ = true;
		}
		this.scheduleExpireIconCache(e);
	}
	declutter(e, t) {
		if (e.declutter) {
			for (let i = t.length - 1; i >= 0; --i) {
				const s = t[i];
				const r = t[i].layer;
				if (t[i].layer.getDeclutter()) {
					s.layer.renderDeclutter(e, t[i]);
				}
			}
			t.forEach((i) => i.layer.renderDeferred(e));
		}
	}
}
function yd(n) {
	if (n instanceof wp) {
		n.setMapInternal(null);
		return;
	}
	if (n instanceof Jl) {
		n.getLayers().forEach(yd);
	}
}
function xd(n, e) {
	if (n instanceof wp) {
		n.setMapInternal(e);
		return;
	}
	if (n instanceof Jl) {
		const t = n.getLayers().getArray();
		for (let i = 0, s = t.length; i < s; ++i) xd(t[i], e);
	}
}
let Dy = class extends Lf {
	constructor(e) {
		super();
		e = e || {};
		this.on;
		this.once;
		this.un;
		const t = Ny(e);
		this.renderComplete_ = false;
		this.loaded_ = true;
		this.boundHandleBrowserEvent_ = this.handleBrowserEvent.bind(this);
		this.maxTilesLoading_ = e.maxTilesLoading !== "undefined" ? e.maxTilesLoading : 16;
		this.pixelRatio_ = e.pixelRatio !== "undefined" ? e.pixelRatio : Lu;
		this.postRenderTimeoutHandle_;
		this.animationDelayKey_;
		this.animationDelay_ = this.animationDelay_.bind(this);
		this.coordinateToPixelTransform_ = Se();
		this.pixelToCoordinateTransform_ = Se();
		this.frameIndex_ = 0;
		this.frameState_ = null;
		this.previousExtent_ = null;
		this.viewPropertyListenerKey_ = null;
		this.viewChangeListenerKey_ = null;
		this.layerGroupPropertyListenerKeys_ = null;
		if (!ht) {
			this.viewport_ = document.createElement("div");
			this.viewport_.className = "ol-viewport" + ("ontouchstart" in window ? " ol-touch" : "");
			this.viewport_.style.position = "relative";
			this.viewport_.style.overflow = "hidden";
			this.viewport_.style.width = "100%";
			this.viewport_.style.height = "100%";
			this.overlayContainer_ = document.createElement("div");
			this.overlayContainer_.style.position = "absolute";
			this.overlayContainer_.style.zIndex = "0";
			this.overlayContainer_.style.width = "100%";
			this.overlayContainer_.style.height = "100%";
			this.overlayContainer_.style.pointerEvents = "none";
			this.overlayContainer_.className = "ol-overlaycontainer";
			this.viewport_.appendChild(this.overlayContainer_);
			this.overlayContainerStopEvent_ = document.createElement("div");
			this.overlayContainerStopEvent_.style.position = "absolute";
			this.overlayContainerStopEvent_.style.zIndex = "0";
			this.overlayContainerStopEvent_.style.width = "100%";
			this.overlayContainerStopEvent_.style.height = "100%";
			this.overlayContainerStopEvent_.style.pointerEvents = "none";
			this.overlayContainerStopEvent_.className = "ol-overlaycontainer-stopevent";
			this.viewport_.appendChild(this.overlayContainerStopEvent_);
		}
		this.mapBrowserEventHandler_ = null;
		this.moveTolerance_ = e.moveTolerance;
		this.keyboardEventTarget_ = t.keyboardEventTarget;
		this.targetChangeHandlerKeys_ = null;
		this.targetElement_ = null;
		if (!ht) {
			this.resizeObserver_ = new ResizeObserver(() => this.updateSize());
		}
		this.controls = t.controls || (ht ? new Af() : qp());
		this.interactions = t.interactions || (ht ? new Af() : Fy({ onFocusOnly: true }));
		this.overlays_ = t.overlays;
		this.overlayIdIndex_ = {};
		this.renderer_ = null;
		this.postRenderFunctions_ = [];
		this.tileQueue_ = new Up(this.getTilePriority.bind(this), this.handleTileChange_.bind(this));
		this.addChangeListener(Ge.LAYERGROUP, this.handleLayerGroupChanged_);
		this.addChangeListener(Ge.VIEW, this.handleViewChanged_);
		this.addChangeListener(Ge.SIZE, this.handleSizeChanged_);
		this.addChangeListener(Ge.TARGET, this.handleTargetChanged_);
		this.setProperties(t.values);
		const i = this;
		if (e.view && !(e.view instanceof Ep)) {
			e.view.then(function(s) {
				i.setView(new Ep(s));
			});
		}
		this.controls.addEventListener(_e.ADD, (s) => {
			s.element.setMap(this);
		});
		this.controls.addEventListener(_e.REMOVE, (s) => {
			s.element.setMap(null);
		});
		this.interactions.addEventListener(_e.ADD, (s) => {
			s.element.setMap(this);
		});
		this.interactions.addEventListener(_e.REMOVE, (s) => {
			s.element.setMap(null);
		});
		this.overlays_.addEventListener(_e.ADD, (s) => {
			this.addOverlayInternal_(s.element);
		});
		this.overlays_.addEventListener(_e.REMOVE, (s) => {
			const r = s.element.getId();
			if (r !== "undefined") {
				delete this.overlayIdIndex_[r.toString()];
			}
			s.element.setMap(null);
		});
		this.controls.forEach((s) => {
			s.setMap(this);
		});
		this.interactions.forEach((s) => {
			s.setMap(this);
		});
		this.overlays_.forEach(this.addOverlayInternal_.bind(this));
	}
	addControl(e) {
		this.getControls().push(e);
	}
	addInteraction(e) {
		this.getInteractions().push(e);
	}
	addLayer(e) {
		this.getLayerGroup().getLayers().push(e);
	}
	handleLayerAdd_(e) {
		xd(e.layer, this);
	}
	addOverlay(e) {
		this.getOverlays().push(e);
	}
	addOverlayInternal_(e) {
		const t = e.getId();
		if (t !== "undefined") {
			this.overlayIdIndex_[t.toString()] = e;
		}
		e.setMap(this);
	}
	disposeInternal() {
		var e;
		this.controls.clear();
		this.interactions.clear();
		this.overlays_.clear();
		if (!((e = this.resizeObserver_) == null)) {
			e.disconnect();
		}
		this.setTarget(null);
		super.disposeInternal();
	}
	forEachFeatureAtPixel(e, t, i) {
		if (!this.frameState_ || !this.renderer_) return;
		const s = this.getCoordinateFromPixelInternal(e);
		i = i !== "undefined" ? i : {};
		const r = i.hitTolerance !== "undefined" ? i.hitTolerance : 0, o = i.layerFilter !== "undefined" ? i.layerFilter : ni, a = i.checkWrapped !== false;
		return this.renderer_.forEachFeatureAtCoordinate(s, this.frameState_, r, a, t, null, o, null);
	}
	getFeaturesAtPixel(e, t) {
		const i = [];
		this.forEachFeatureAtPixel(e, function(s) {
			i.push(s);
		}, t);
		return i;
	}
	getAllLayers() {
		const e = [];
		function t(i) {
			i.forEach(function(s) {
				s instanceof Jl ? t(s.getLayers()) : e.push(s);
			});
		}
		t(this.getLayers());
		return e;
	}
	hasFeatureAtPixel(e, t) {
		if (!this.frameState_ || !this.renderer_) return false;
		const i = this.getCoordinateFromPixelInternal(e);
		t = t !== "undefined" ? t : {};
		const s = t.layerFilter !== "undefined" ? t.layerFilter : ni, r = t.hitTolerance !== "undefined" ? t.hitTolerance : 0, o = t.checkWrapped !== false;
		return this.renderer_.hasFeatureAtCoordinate(i, this.frameState_, r, o, s, null);
	}
	getEventCoordinate(e) {
		return this.getCoordinateFromPixel(this.getEventPixel(e));
	}
	getEventCoordinateInternal(e) {
		return this.getCoordinateFromPixelInternal(this.getEventPixel(e));
	}
	getEventPixel(e) {
		const i = this.viewport_.getBoundingClientRect(), s = this.getSize(), r = i.width / s[0], o = i.height / s[1], a = "changedTouches" in e ? e.changedTouches[0] : e;
		return [(a.clientX - i.left) / r, (a.clientY - i.top) / o];
	}
	getTarget() {
		return this.get(Ge.TARGET);
	}
	getTargetElement() {
		return this.targetElement_;
	}
	getCoordinateFromPixel(e) {
		return vi(this.getCoordinateFromPixelInternal(e), this.getView().getProjection());
	}
	getCoordinateFromPixelInternal(e) {
		const t = this.frameState_;
		return this.frameState_ ? xe(this.frameState_.pixelToCoordinateTransform, e.slice()) : null;
	}
	getControls() {
		return this.controls;
	}
	getOverlays() {
		return this.overlays_;
	}
	getOverlayById(e) {
		const t = this.overlayIdIndex_[e.toString()];
		return this.overlayIdIndex_[e.toString()] !== "undefined" ? this.overlayIdIndex_[e.toString()] : null;
	}
	getInteractions() {
		return this.interactions;
	}
	getLayerGroup() {
		return this.get(Ge.LAYERGROUP);
	}
	setLayers(e) {
		const t = this.getLayerGroup();
		if (e instanceof Af) {
			t.setLayers(e);
			return;
		}
		const i = t.getLayers();
		i.clear();
		i.extend(e);
	}
	getLayers() {
		return this.getLayerGroup().getLayers();
	}
	getLoadingOrNotReady() {
		const e = this.getLayerGroup().getLayerStatesArray();
		for (let t = 0, i = e.length; t < i; ++t) {
			const s = e[t];
			if (!e[t].visible) continue;
			const r = e[t].layer.getRenderer();
			if (r && !r.ready) return true;
			const o = e[t].layer.getSource();
			if (o && o.loading) return true;
		}
		return false;
	}
	getPixelFromCoordinate(e) {
		const t = ue(e, this.getView().getProjection());
		return this.getPixelFromCoordinateInternal(t);
	}
	getPixelFromCoordinateInternal(e) {
		const t = this.frameState_;
		return this.frameState_ ? xe(this.frameState_.coordinateToPixelTransform, e.slice(0, 2)) : null;
	}
	getPixelRatio() {
		return this.pixelRatio_;
	}
	setPixelRatio(e) {
		if (this.pixelRatio_ !== e) {
			this.pixelRatio_ = e;
			this.render();
		}
	}
	getRenderer() {
		return this.renderer_;
	}
	getSize() {
		return this.get(Ge.SIZE);
	}
	getView() {
		return this.get(Ge.VIEW);
	}
	getViewport() {
		return this.viewport_;
	}
	getOverlayContainer() {
		return this.overlayContainer_;
	}
	getOverlayContainerStopEvent() {
		return this.overlayContainerStopEvent_;
	}
	getOwnerDocument() {
		const e = this.getTargetElement();
		return e ? e.ownerDocument : document;
	}
	getTilePriority(e, t, i, s) {
		return zp(this.frameState_, e, t, i, s);
	}
	handleBrowserEvent(e, t) {
		t = t || e.type;
		const i = new Np(t, this, e);
		this.handleMapBrowserEvent(i);
	}
	handleMapBrowserEvent(e) {
		if (!this.frameState_) return;
		const t = e.originalEvent, i = e.originalEvent.type;
		if (e.originalEvent.type === Oa.POINTERDOWN || e.originalEvent.type === U.WHEEL || e.originalEvent.type === U.KEYDOWN) {
			const s = this.getOwnerDocument();
			const r = this.viewport_.getRootNode ? this.viewport_.getRootNode() : s;
			const o = t.target;
			const a = r instanceof ShadowRoot ? r.host === t.target ? r.host.ownerDocument : r : r === s ? s.documentElement : r;
			if (this.overlayContainerStopEvent_.contains(t.target) || !a.contains(t.target)) return;
		}
		e.frameState = this.frameState_;
		if (this.dispatchEvent(e) !== false) {
			const s = this.getInteractions().getArray().slice();
			for (let r = s.length - 1; r >= 0; r--) {
				const o = s[r];
				if (s[r].getMap() !== this || !s[r].getActive() || !this.getTargetElement()) continue;
				if (!s[r].handleEvent(e) || e.propagationStopped) break;
			}
		}
	}
	handlePostRender() {
		const e = this.frameState_, t = this.tileQueue_;
		if (!this.tileQueue_.isEmpty()) {
			let s = this.maxTilesLoading_;
			let r = s;
			if (e) {
				const o = e.viewHints;
				if (e.viewHints[de.ANIMATING] || e.viewHints[de.INTERACTING]) {
					const a = Date.now() - e.time > 8;
					s = a ? 0 : 8;
					r = a ? 0 : 2;
				}
			}
			if (t.getTilesLoading() < s) {
				t.reprioritize();
				t.loadMoreTiles(s, r);
			}
		}
		if (this.frameState_ && this.renderer_ && !this.frameState_.animate) {
			this.renderComplete_ ? (this.hasListener(Me.RENDERCOMPLETE) && this.renderer_.dispatchRenderEvent(Me.RENDERCOMPLETE, this.frameState_), this.loaded_ === false && (this.loaded_ = true, this.dispatchEvent(new Dp(Zt.LOADEND, this, this.frameState_)))) : this.loaded_ === true && (this.loaded_ = false, this.dispatchEvent(new Dp(Zt.LOADSTART, this, this.frameState_)));
		}
		const i = this.postRenderFunctions_;
		if (this.frameState_) for (let s = 0, r = this.postRenderFunctions_.length; s < r; ++s) this.postRenderFunctions_[s](this, this.frameState_);
		this.postRenderFunctions_.length = 0;
	}
	handleSizeChanged_() {
		if (this.getView() && !this.getView().getAnimating()) {
			this.getView().resolveConstraints(0);
		}
		this.render();
	}
	handleTargetChanged_() {
		var i, s;
		if (this.mapBrowserEventHandler_) {
			for (let r = 0, o = this.targetChangeHandlerKeys_.length; r < o; ++r) se(this.targetChangeHandlerKeys_[r]);
			this.targetChangeHandlerKeys_ = null;
			this.viewport_.removeEventListener(U.CONTEXTMENU, this.boundHandleBrowserEvent_);
			this.viewport_.removeEventListener(U.WHEEL, this.boundHandleBrowserEvent_);
			this.mapBrowserEventHandler_.dispose();
			this.mapBrowserEventHandler_ = null;
			this.viewport_.remove();
		}
		if (this.targetElement_ && !pi(this.targetElement_)) {
			if (!((i = this.resizeObserver_) == null)) {
				i.unobserve(this.targetElement_);
			}
			const r = this.targetElement_.getRootNode();
			if (r instanceof ShadowRoot) {
				this.resizeObserver_.unobserve(r.host);
			}
			this.setSize("undefined");
		}
		const e = this.getTarget(), t = typeof e == "string" ? document.getElementById(e) : e;
		this.targetElement_ = t;
		if (!t) {
			if (this.renderer_) {
				clearTimeout(this.postRenderTimeoutHandle_);
				this.postRenderTimeoutHandle_ = "undefined";
				this.postRenderFunctions_.length = 0;
				this.renderer_.dispose();
				this.renderer_ = null;
			}
			if (this.animationDelayKey_) {
				cancelAnimationFrame(this.animationDelayKey_);
				this.animationDelayKey_ = "undefined";
			}
		} else {
			if (!pi(t)) {
				t.appendChild(this.viewport_);
			}
			if (!this.renderer_) {
				this.renderer_ = new by(this);
			}
			if (!pi(t)) {
				this.mapBrowserEventHandler_ = new kp(this, this.moveTolerance_);
				for (const o in Q) this.mapBrowserEventHandler_.addEventListener(Q[o], this.handleMapBrowserEvent.bind(this));
				this.viewport_.addEventListener(U.CONTEXTMENU, this.boundHandleBrowserEvent_, false);
				this.viewport_.addEventListener(U.WHEEL, this.boundHandleBrowserEvent_, Mu ? { passive: false } : false);
				let r;
				if (this.keyboardEventTarget_) r = this.keyboardEventTarget_;
				else {
					const o = t.getRootNode();
					r = o instanceof ShadowRoot ? o.host : t;
				}
				this.targetChangeHandlerKeys_ = [Z(r, U.KEYDOWN, this.handleBrowserEvent, this), Z(r, U.KEYPRESS, this.handleBrowserEvent, this)];
				if (t instanceof HTMLElement) {
					const o = t.getRootNode();
					if (o instanceof ShadowRoot) {
						this.resizeObserver_.observe(o.host);
					}
					if (!((s = this.resizeObserver_) == null)) {
						s.observe(t);
					}
				}
			}
			this.updateSize();
		}
	}
	handleTileChange_() {
		this.render();
	}
	handleViewPropertyChanged_() {
		this.render();
	}
	handleViewChanged_() {
		if (this.viewPropertyListenerKey_) {
			se(this.viewPropertyListenerKey_);
			this.viewPropertyListenerKey_ = null;
		}
		if (this.viewChangeListenerKey_) {
			se(this.viewChangeListenerKey_);
			this.viewChangeListenerKey_ = null;
		}
		const e = this.getView();
		if (e) {
			this.updateViewportSize_(this.getSize());
			this.viewPropertyListenerKey_ = Z(e, xt.PROPERTYCHANGE, this.handleViewPropertyChanged_, this);
			this.viewChangeListenerKey_ = Z(e, U.CHANGE, this.handleViewPropertyChanged_, this);
			e.resolveConstraints(0);
		}
		this.render();
	}
	handleLayerGroupChanged_() {
		if (this.layerGroupPropertyListenerKeys_) {
			this.layerGroupPropertyListenerKeys_.forEach(se);
			this.layerGroupPropertyListenerKeys_ = null;
		}
		const e = this.getLayerGroup();
		if (e) {
			this.handleLayerAdd_(new gi("addlayer", e));
			this.layerGroupPropertyListenerKeys_ = [
				Z(e, xt.PROPERTYCHANGE, this.render, this),
				Z(e, U.CHANGE, this.render, this),
				Z(e, "addlayer", this.handleLayerAdd_, this),
				Z(e, "removelayer", this.handleLayerRemove_, this)
			];
		}
		this.render();
	}
	isRendered() {
		return !!this.frameState_;
	}
	animationDelay_() {
		this.animationDelayKey_ = "undefined";
		this.renderFrame_(Date.now());
	}
	renderSync() {
		if (this.animationDelayKey_) {
			cancelAnimationFrame(this.animationDelayKey_);
		}
		this.animationDelay_();
	}
	redrawText() {
		if (!this.frameState_) return;
		const e = this.frameState_.layerStatesArray;
		for (let t = 0, i = this.frameState_.layerStatesArray.length; t < i; ++t) {
			const s = e[t].layer;
			if (e[t].layer.hasRenderer()) {
				e[t].layer.getRenderer().handleFontsChanged();
			}
		}
	}
	render() {
		if (this.renderer_ && this.animationDelayKey_ === "undefined") {
			this.animationDelayKey_ = requestAnimationFrame(this.animationDelay_);
		}
	}
	removeControl(e) {
		return this.getControls().remove(e);
	}
	removeInteraction(e) {
		return this.getInteractions().remove(e);
	}
	removeLayer(e) {
		return this.getLayerGroup().getLayers().remove(e);
	}
	handleLayerRemove_(e) {
		yd(e.layer);
	}
	removeOverlay(e) {
		return this.getOverlays().remove(e);
	}
	renderFrame_(e) {
		const t = this.getSize(), i = this.getView(), s = this.frameState_;
		let r = null;
		if (t !== "undefined" && sc(t) && i && i.isDef()) {
			const o = i.getHints(this.frameState_ ? this.frameState_.viewHints : "undefined");
			const a = i.getState();
			r = {
				animate: false,
				coordinateToPixelTransform: this.coordinateToPixelTransform_,
				declutter: null,
				extent: gs(a.center, a.resolution, a.rotation, t),
				index: this.frameIndex_++,
				layerIndex: 0,
				layerStatesArray: this.getLayerGroup().getLayerStatesArray(),
				pixelRatio: this.pixelRatio_,
				pixelToCoordinateTransform: this.pixelToCoordinateTransform_,
				postRenderFunctions: [],
				size: t,
				tileQueue: this.tileQueue_,
				time: e,
				usedTiles: {},
				viewState: a,
				viewHints: o,
				wantedTiles: {},
				mapId: O(this),
				renderTargets: {}
			};
			if (a.nextCenter && a.nextResolution) {
				const l = isNaN(a.nextRotation) ? a.rotation : a.nextRotation;
				r.nextExtent = gs(a.nextCenter, a.nextResolution, l, t);
			}
		}
		this.frameState_ = r;
		this.renderer_.renderFrame(r);
		this.dispatchEvent(new Dp(Zt.POSTRENDER, this, r));
		this.renderComplete_ = (this.hasListener(Zt.LOADSTART) || this.hasListener(Zt.LOADEND) || this.hasListener(Me.RENDERCOMPLETE)) && !this.tileQueue_.getTilesLoading() && !this.tileQueue_.getCount() && !this.getLoadingOrNotReady();
		if (!this.postRenderTimeoutHandle_) {
			this.postRenderTimeoutHandle_ = setTimeout(() => {
				this.postRenderTimeoutHandle_ = "undefined";
				this.handlePostRender();
			}, 0);
		}
	}
	setLayerGroup(e) {
		const t = this.getLayerGroup();
		if (t) {
			this.handleLayerRemove_(new gi("removelayer", t));
		}
		this.set(Ge.LAYERGROUP, e);
	}
	setSize(e) {
		this.set(Ge.SIZE, e);
	}
	setTarget(e) {
		this.set(Ge.TARGET, e);
	}
	setView(e) {
		if (!e || e instanceof Ep) {
			this.set(Ge.VIEW, e);
			return;
		}
		this.set(Ge.VIEW, new Ep());
		const t = this;
		e.then(function(i) {
			t.setView(new Ep(i));
		});
	}
	updateSize() {
		const e = this.getTargetElement();
		let t;
		if (e) {
			let s;
			let r;
			if (pi(e)) {
				const o = e.getContext("2d").getTransform();
				s = e.width / o.a;
				r = e.height / o.d;
			} else {
				const o = getComputedStyle(e);
				s = e.offsetWidth - parseFloat(o.borderLeftWidth) - parseFloat(o.paddingLeft) - parseFloat(o.paddingRight) - parseFloat(o.borderRightWidth);
				r = e.offsetHeight - parseFloat(o.borderTopWidth) - parseFloat(o.paddingTop) - parseFloat(o.paddingBottom) - parseFloat(o.borderBottomWidth);
			}
			if (!isNaN(s) && !isNaN(r)) {
				t = [Math.max(0, s), Math.max(0, r)];
				if (!sc(t) && (e.offsetWidth || e.offsetHeight || e.getClientRects().length)) {
					nu("No map visible because the map container's width or height are 0.");
				}
			}
		}
		const i = this.getSize();
		if (t && (!i || !kt(t, i))) {
			this.setSize(t);
			this.updateViewportSize_(t);
		}
	}
	updateViewportSize_(e) {
		const t = this.getView();
		if (t) {
			t.setViewportSize(e);
		}
	}
};
function Ny(n) {
	let e = null;
	if (n.keyboardEventTarget !== "undefined") {
		e = typeof n.keyboardEventTarget == "string" ? document.getElementById(n.keyboardEventTarget) : n.keyboardEventTarget;
	}
	const t = {}, i = n.layers && typeof n.layers.getLayers == "function" ? n.layers : new Jl({ layers: n.layers });
	t[Ge.LAYERGROUP] = i;
	t[Ge.TARGET] = n.target;
	t[Ge.VIEW] = n.view instanceof Ep ? n.view : new Ep();
	let s;
	if (n.controls !== "undefined") {
		Array.isArray(n.controls) ? s = new Af(n.controls.slice()) : (ee(typeof n.controls.getArray == "function", "Expected `controls` to be an array or an `ol/Collection.js`"), s = n.controls);
	}
	let r;
	if (n.interactions !== "undefined") {
		Array.isArray(n.interactions) ? r = new Af(n.interactions.slice()) : (ee(typeof n.interactions.getArray == "function", "Expected `interactions` to be an array or an `ol/Collection.js`"), r = n.interactions);
	}
	let o;
	n.overlays !== "undefined" ? Array.isArray(n.overlays) ? o = new Af(n.overlays.slice()) : (ee(typeof n.overlays.getArray == "function", "Expected `overlays` to be an array or an `ol/Collection.js`"), o = n.overlays) : o = new Af();
	return {
		controls: s,
		interactions: r,
		keyboardEventTarget: e,
		overlays: o,
		values: t
	};
}
const ke = {
	ELEMENT: "element",
	MAP: "map",
	OFFSET: "offset",
	POSITION: "position",
	POSITIONING: "positioning"
};
class ky extends Lf {
	constructor(e) {
		super();
		this.on;
		this.once;
		this.un;
		this.options = e;
		this.id = e.id;
		this.insertFirst = e.insertFirst !== "undefined" ? e.insertFirst : true;
		this.stopEvent = e.stopEvent !== "undefined" ? e.stopEvent : true;
		this.element = document.createElement("div");
		this.element.className = e.className !== "undefined" ? e.className : "ol-overlay-container " + A_;
		this.element.style.position = "absolute";
		this.element.style.pointerEvents = "auto";
		this.autoPan = e.autoPan === true ? {} : e.autoPan || "undefined";
		this.rendered = {
			transform_: "",
			visible: true
		};
		this.mapPostrenderListenerKey = null;
		this.addChangeListener(ke.ELEMENT, this.handleElementChanged);
		this.addChangeListener(ke.MAP, this.handleMapChanged);
		this.addChangeListener(ke.OFFSET, this.handleOffsetChanged);
		this.addChangeListener(ke.POSITION, this.handlePositionChanged);
		this.addChangeListener(ke.POSITIONING, this.handlePositioningChanged);
		if (e.element !== "undefined") {
			this.setElement(e.element);
		}
		this.setOffset(e.offset !== "undefined" ? e.offset : [0, 0]);
		this.setPositioning(e.positioning || "top-left");
		if (e.position !== "undefined") {
			this.setPosition(e.position);
		}
	}
	getElement() {
		return this.get(ke.ELEMENT);
	}
	getId() {
		return this.id;
	}
	getMap() {
		return this.get(ke.MAP) || null;
	}
	getOffset() {
		return this.get(ke.OFFSET);
	}
	getPosition() {
		return this.get(ke.POSITION);
	}
	getPositioning() {
		return this.get(ke.POSITIONING);
	}
	handleElementChanged() {
		bu(this.element);
		const e = this.getElement();
		if (e) {
			this.element.appendChild(e);
		}
	}
	handleMapChanged() {
		var t;
		if (this.mapPostrenderListenerKey) {
			if (!((t = this.element) == null)) {
				t.remove();
			}
			se(this.mapPostrenderListenerKey);
			this.mapPostrenderListenerKey = null;
		}
		const e = this.getMap();
		if (e) {
			this.mapPostrenderListenerKey = Z(e, Zt.POSTRENDER, this.render, this);
			this.updatePixelPosition();
			const i = this.stopEvent ? e.getOverlayContainerStopEvent() : e.getOverlayContainer();
			this.insertFirst ? i.insertBefore(this.element, i.childNodes[0] || null) : i.appendChild(this.element);
			this.performAutoPan();
		}
	}
	render() {
		this.updatePixelPosition();
	}
	handleOffsetChanged() {
		this.updatePixelPosition();
	}
	handlePositionChanged() {
		this.updatePixelPosition();
		this.performAutoPan();
	}
	handlePositioningChanged() {
		this.updatePixelPosition();
	}
	setElement(e) {
		this.set(ke.ELEMENT, e);
	}
	setMap(e) {
		this.set(ke.MAP, e);
	}
	setOffset(e) {
		this.set(ke.OFFSET, e);
	}
	setPosition(e) {
		this.set(ke.POSITION, e);
	}
	performAutoPan() {
		if (this.autoPan) {
			this.panIntoView(this.autoPan);
		}
	}
	panIntoView(e) {
		const t = this.getMap();
		if (!t || !t.getTargetElement() || !this.get(ke.POSITION)) return;
		const i = this.getRect(t.getTargetElement(), t.getSize()), s = this.getElement(), r = this.getRect(s, [d_(s), f_(s)]);
		e = e || {};
		const o = e.margin === "undefined" ? 20 : e.margin;
		if (!at(i, r)) {
			const a = r[0] - i[0];
			const l = i[2] - r[2];
			const h = r[1] - i[1];
			const c = i[3] - r[3];
			const u = [0, 0];
			a < 0 ? u[0] = a - o : l < 0 && (u[0] = Math.abs(l) + o);
			h < 0 ? u[1] = h - o : c < 0 && (u[1] = Math.abs(c) + o);
			if (u[0] !== 0 || u[1] !== 0) {
				const d = t.getView().getCenterInternal();
				const f = t.getPixelFromCoordinateInternal(d);
				if (!f) return;
				const g = [f[0] + u[0], f[1] + u[1]];
				const m = e.animation || {};
				t.getView().animateInternal({
					center: t.getCoordinateFromPixelInternal(g),
					duration: m.duration,
					easing: m.easing
				});
			}
		}
	}
	getRect(e, t) {
		const i = e.getBoundingClientRect(), s = i.left + window.pageXOffset, r = i.top + window.pageYOffset;
		return [
			s,
			r,
			s + t[0],
			r + t[1]
		];
	}
	setPositioning(e) {
		this.set(ke.POSITIONING, e);
	}
	setVisible(e) {
		if (this.rendered.visible !== e) {
			this.element.style.display = e ? "" : "none";
			this.rendered.visible = e;
		}
	}
	updatePixelPosition() {
		const e = this.getMap(), t = this.getPosition();
		if (!e || !e.isRendered() || !t) {
			this.setVisible(false);
			return;
		}
		const i = e.getPixelFromCoordinate(t), s = e.getSize();
		this.updateRenderedPosition(i, s);
	}
	updateRenderedPosition(e, t) {
		const i = this.element.style, s = this.getOffset(), r = this.getPositioning();
		this.setVisible(true);
		const o = "".concat(e[0] + s[0], "px"), a = "".concat(e[1] + s[1], "px");
		let l = "0%", h = "0%";
		r == "bottom-right" || r == "center-right" || r == "top-right" ? l = "-100%" : (r == "bottom-center" || r == "center-center" || r == "top-center") && (l = "-50%");
		r == "bottom-left" || r == "bottom-center" || r == "bottom-right" ? h = "-100%" : (r == "center-left" || r == "center-center" || r == "center-right") && (h = "-50%");
		const c = "translate(".concat(l, ", ").concat(h, ") translate(").concat(o, ", ").concat(a, ")");
		if (this.rendered.transform_ != c) {
			this.rendered.transform_ = c;
			this.element.style.transform = c;
		}
	}
	getOptions() {
		return this.options;
	}
}
class Ed {
	constructor(e, t, i, s) {
		this.minX = e;
		this.maxX = t;
		this.minY = i;
		this.maxY = s;
	}
	contains(e) {
		return this.containsXY(e[1], e[2]);
	}
	containsTileRange(e) {
		return this.minX <= e.minX && e.maxX <= this.maxX && this.minY <= e.minY && e.maxY <= this.maxY;
	}
	containsXY(e, t) {
		return this.minX <= e && e <= this.maxX && this.minY <= t && t <= this.maxY;
	}
	equals(e) {
		return this.minX == e.minX && this.minY == e.minY && this.maxX == e.maxX && this.maxY == e.maxY;
	}
	extend(e) {
		if (e.minX < this.minX) {
			this.minX = e.minX;
		}
		if (e.maxX > this.maxX) {
			this.maxX = e.maxX;
		}
		if (e.minY < this.minY) {
			this.minY = e.minY;
		}
		if (e.maxY > this.maxY) {
			this.maxY = e.maxY;
		}
	}
	getHeight() {
		return this.maxY - this.minY + 1;
	}
	getSize() {
		return [this.getWidth(), this.getHeight()];
	}
	getWidth() {
		return this.maxX - this.minX + 1;
	}
	intersects(e) {
		return this.minX <= e.maxX && this.maxX >= e.minX && this.minY <= e.maxY && this.maxY >= e.minY;
	}
}
function nn(n, e, t, i, s) {
	return s !== "undefined" ? (s.minX = n, s.maxX = e, s.minY = t, s.maxY = i, s) : new Ed(n, e, t, i);
}
const Rc = [];
class Gy extends Lp {
	constructor(e, t, i, s, r) {
		super(e, t, { transition: 0 });
		this.context_ = null;
		this.executorGroups = {};
		this.loadingSourceTiles = 0;
		this.hitDetectionImageData = {};
		this.replayState_ = {};
		this.sourceTiles = [];
		this.errorTileKeys = {};
		this.wantedResolution;
		this.getSourceTiles = s.bind("undefined", this);
		this.removeSourceTiles_ = r;
		this.wrappedTileCoord = i;
	}
	getContext() {
		if (!this.context_) {
			this.context_ = Re(1, 1, Rc);
		}
		return this.context_;
	}
	hasContext() {
		return !!this.context_;
	}
	getImage() {
		return this.hasContext() ? this.getContext().canvas : null;
	}
	getReplayState(e) {
		const t = O(e);
		if (!(t in this.replayState_)) {
			this.replayState_[t] = {
				dirty: false,
				renderedRenderOrder: null,
				renderedResolution: NaN,
				renderedPixelRatio: NaN,
				renderedRevision: -1,
				renderedTileResolution: NaN,
				renderedTileRevision: -1,
				renderedTileZ: -1
			};
		}
		return this.replayState_[t];
	}
	load() {
		this.getSourceTiles();
	}
	release() {
		if (this.context_) {
			Ns(this.context_);
			Rc.push(this.context_.canvas);
			this.context_ = null;
		}
		this.removeSourceTiles_(this);
		this.sourceTiles.length = 0;
		super.release();
	}
}
let $y = class extends Lp {
	constructor(e, t, i, s, r, o) {
		super(e, t, o);
		this.extent = null;
		this.format_ = s;
		this.features_ = null;
		this.loader_;
		this.projection = null;
		this.resolution;
		this.tileLoadFunction_ = r;
		this.url_ = i;
		this.key = i;
	}
	getTileUrl() {
		return this.url_;
	}
	getFormat() {
		return this.format_;
	}
	getFeatures() {
		return this.features_;
	}
	load() {
		if (this.state == M.IDLE) {
			this.setState(M.LOADING);
			this.tileLoadFunction_(this, this.url_);
			if (this.loader_) {
				this.loader_(this.extent, this.resolution, this.projection);
			}
		}
	}
	onLoad(e, t) {
		this.setFeatures(e);
	}
	onError() {
		this.setState(M.ERROR);
	}
	setFeatures(e) {
		this.features_ = e;
		this.setState(M.LOADED);
	}
	setLoader(e) {
		this.loader_ = e;
	}
};
class Ql {
	constructor() {
		this.dataProjection = "undefined";
		this.defaultFeatureProjection = "undefined";
		this.featureClass = sl;
		this.supportedMediaTypes = null;
	}
	getReadOptions(e, t) {
		if (t) {
			let i = t.dataProjection ? H(t.dataProjection) : this.readProjection(e);
			if (t.extent && i && i.getUnits() === "tile-pixels") {
				i = H(i);
				i.setWorldExtent(t.extent);
			}
			t = {
				dataProjection: i,
				featureProjection: t.featureProjection
			};
		}
		return this.adaptOptions(t);
	}
	adaptOptions(e) {
		return Object.assign({
			dataProjection: this.dataProjection,
			featureProjection: this.defaultFeatureProjection,
			featureClass: this.featureClass
		}, e);
	}
	getType() {
		return z();
	}
	readFeature(e, t) {
		return z();
	}
	readFeatures(e, t) {
		return z();
	}
	readGeometry(e, t) {
		return z();
	}
	readProjection(e) {
		return z();
	}
	writeFeature(e, t) {
		return z();
	}
	writeFeatures(e, t) {
		return z();
	}
	writeGeometry(e, t) {
		return z();
	}
}
function wi(n, e, t) {
	const i = t ? H(t.featureProjection) : null, s = t ? H(t.dataProjection) : null;
	let r = n;
	if (i && s && !Ae(i, s)) {
		if (e) {
			r = n.clone();
		}
		const o = e ? i : s;
		const a = e ? s : i;
		o.getUnits() === "tile-pixels" ? r.transform(o, a) : r.applyTransform(ji(o, a));
	}
	if (e && t && t.decimals !== "undefined") {
		const o = Math.pow(10, t.decimals);
		const a = function(l) {
			for (let h = 0, c = l.length; h < c; ++h) l[h] = Math.round(l[h] * o) / o;
			return l;
		};
		if (r === n) {
			r = n.clone();
		}
		r.applyTransform(a);
	}
	return r;
}
const jy = {
	Point: pl,
	LineString: Nr,
	Polygon: wn,
	MultiPoint: yl,
	MultiLineString: kr,
	MultiPolygon: Gr
};
function zy(n, e, t) {
	return Array.isArray(e[0]) ? (du(n, 0, e, t) || (n = n.slice(), va(n, 0, e, t)), n) : (al(n, 0, e, t) || (n = n.slice(), Ar(n, 0, e, t)), n);
}
function Cd(n, e) {
	var r;
	const t = n.geometry;
	if (!n.geometry) return [];
	if (Array.isArray(n.geometry)) return n.geometry.map((o) => Cd({
		...n,
		geometry: o
	})).flat();
	const i = n.geometry.type === "MultiPolygon" ? "Polygon" : n.geometry.type;
	if (i === "GeometryCollection" || i === "Circle") throw new Error("Unsupported geometry type: " + i);
	const s = n.geometry.layout.length;
	return wi(new Pn(i, i === "Polygon" ? zy(n.geometry.flatCoordinates, n.geometry.ends, n.geometry.layout.length) : n.geometry.flatCoordinates, (r = n.geometry.ends) == null ? "undefined" : r.flat(), n.geometry.layout.length, n.properties || {}, n.id).enableSimplifyTransformed(), false, e);
}
function eh(n, e) {
	if (!n) return null;
	if (Array.isArray(n)) {
		const i = n.map((s) => eh(s, e));
		return new br(i);
	}
	const t = jy[n.type];
	return wi(new jy[n.type](n.flatCoordinates, n.layout || "XY", n.ends), false, e);
}
class Xy extends Ql {
	constructor() {
		super();
	}
	getType() {
		return "json";
	}
	readFeature(e, t) {
		return this.readFeatureFromObject(rr(e), this.getReadOptions(e, t));
	}
	readFeatures(e, t) {
		return this.readFeaturesFromObject(rr(e), this.getReadOptions(e, t));
	}
	readFeatureFromObject(e, t) {
		return z();
	}
	readFeaturesFromObject(e, t) {
		return z();
	}
	readGeometry(e, t) {
		return this.readGeometryFromObject(rr(e), this.getReadOptions(e, t));
	}
	readGeometryFromObject(e, t) {
		return z();
	}
	readProjection(e) {
		return this.readProjectionFromObject(rr(e));
	}
	readProjectionFromObject(e) {
		return z();
	}
	writeFeature(e, t) {
		return JSON.stringify(this.writeFeatureObject(e, t));
	}
	writeFeatureObject(e, t) {
		return z();
	}
	writeFeatures(e, t) {
		return JSON.stringify(this.writeFeaturesObject(e, t));
	}
	writeFeaturesObject(e, t) {
		return z();
	}
	writeGeometry(e, t) {
		return JSON.stringify(this.writeGeometryObject(e, t));
	}
	writeGeometryObject(e, t) {
		return z();
	}
}
function rr(n) {
	if (typeof n == "string") {
		const e = JSON.parse(n);
		return e || null;
	}
	return n !== null ? n : null;
}
class Wy extends Xy {
	constructor(e) {
		e = e || {};
		super();
		this.dataProjection = H(e.dataProjection ? e.dataProjection : "EPSG:4326");
		if (e.featureProjection) {
			this.defaultFeatureProjection = H(e.featureProjection);
		}
		if (e.featureClass) {
			this.featureClass = e.featureClass;
		}
		this.geometryName_ = e.geometryName;
		this.extractGeometryName_ = e.extractGeometryName;
		this.supportedMediaTypes = ["application/geo+json", "application/vnd.geo+json"];
	}
	readFeatureFromObject(e, t) {
		let i = null;
		e.type === "Feature" ? i = e : i = {
			type: "Feature",
			geometry: e,
			properties: null
		};
		const s = th(i.geometry);
		if (this.featureClass === Pn) return Cd({
			geometry: s,
			id: i.id,
			properties: i.properties
		}, t);
		const r = new sl();
		this.geometryName_ ? r.setGeometryName(this.geometryName_) : this.extractGeometryName_ && i.geometry_name && r.setGeometryName(i.geometry_name);
		r.setGeometry(eh(s, t));
		if ("id" in i) {
			r.setId(i.id);
		}
		if (i.properties) {
			r.setProperties(i.properties, true);
		}
		return r;
	}
	readFeaturesFromObject(e, t) {
		const i = e;
		let s = null;
		if (e.type === "FeatureCollection") {
			const r = e;
			s = [];
			const o = e.features;
			for (let a = 0, l = e.features.length; a < l; ++a) {
				const h = this.readFeatureFromObject(o[a], t);
				if (h) {
					s.push(h);
				}
			}
		} else s = [this.readFeatureFromObject(e, t)];
		return s.flat();
	}
	readGeometryFromObject(e, t) {
		return Vy(e, t);
	}
	readProjectionFromObject(e) {
		const t = e.crs;
		let i;
		if (e.crs) if (e.crs.type == "name") i = H(e.crs.properties.name);
		else if (e.crs.type === "EPSG") i = H("EPSG:" + e.crs.properties.code);
		else throw new Error("Unknown SRS type");
		else i = this.dataProjection;
		return i;
	}
	writeFeatureObject(e, t) {
		t = this.adaptOptions(t);
		const i = {
			type: "Feature",
			geometry: null,
			properties: null
		}, s = e.getId();
		if (s !== "undefined") {
			i.id = s;
		}
		if (!e.hasProperties()) return i;
		const r = e.getProperties(), o = e.getGeometry();
		if (o) {
			i.geometry = ka(o, t);
			delete r[e.getGeometryName()];
		}
		if (!si(r)) {
			i.properties = r;
		}
		return i;
	}
	writeFeaturesObject(e, t) {
		t = this.adaptOptions(t);
		const i = [];
		for (let s = 0, r = e.length; s < r; ++s) i.push(this.writeFeatureObject(e[s], t));
		return {
			type: "FeatureCollection",
			features: i
		};
	}
	writeGeometryObject(e, t) {
		return ka(e, this.adaptOptions(t));
	}
}
function th(n, e) {
	if (!n) return null;
	let t;
	switch (n.type) {
		case "Point": {
			t = Zy(n);
			break;
		}
		case "LineString": {
			t = Ky(n);
			break;
		}
		case "Polygon": {
			t = Qy(n);
			break;
		}
		case "MultiPoint": {
			t = qy(n);
			break;
		}
		case "MultiLineString": {
			t = Hy(n);
			break;
		}
		case "MultiPolygon": {
			t = Jy(n);
			break;
		}
		case "GeometryCollection": {
			t = Yy(n);
			break;
		}
		default: throw new Error("Unsupported GeoJSON type: " + n.type);
	}
	return t;
}
function Vy(n, e) {
	const t = th(n);
	return eh(t, e);
}
function Yy(n, e) {
	return n.geometries.map(function(i) {
		return th(i);
	});
}
function Zy(n) {
	const e = n.coordinates;
	return {
		type: "Point",
		flatCoordinates: n.coordinates,
		layout: Yi(n.coordinates.length)
	};
}
function Ky(n) {
	var i;
	const e = n.coordinates, t = n.coordinates.flat();
	return {
		type: "LineString",
		flatCoordinates: t,
		ends: [t.length],
		layout: Yi(((i = n.coordinates[0]) == null ? "undefined" : i.length) || 2)
	};
}
function Hy(n) {
	var r, o;
	const e = n.coordinates, t = ((o = (r = n.coordinates[0]) == null ? "undefined" : r[0]) == null ? "undefined" : o.length) || 2, i = [], s = Ds(i, 0, n.coordinates, t);
	return {
		type: "MultiLineString",
		flatCoordinates: i,
		ends: s,
		layout: Yi(t)
	};
}
function qy(n) {
	var t;
	const e = n.coordinates;
	return {
		type: "MultiPoint",
		flatCoordinates: n.coordinates.flat(),
		layout: Yi(((t = n.coordinates[0]) == null ? "undefined" : t.length) || 2)
	};
}
function Jy(n) {
	var r, o;
	const e = n.coordinates, t = [], i = ((o = (r = n.coordinates[0]) == null ? "undefined" : r[0]) == null ? "undefined" : o[0].length) || 2, s = xu(t, 0, n.coordinates, i);
	return {
		type: "MultiPolygon",
		flatCoordinates: t,
		ends: s,
		layout: Yi(i)
	};
}
function Qy(n) {
	var r, o;
	const e = n.coordinates, t = [], i = (o = (r = n.coordinates[0]) == null ? "undefined" : r[0]) == null ? "undefined" : o.length, s = Ds(t, 0, n.coordinates, i);
	return {
		type: "Polygon",
		flatCoordinates: t,
		ends: s,
		layout: Yi(i)
	};
}
function ka(n, e) {
	n = wi(n, true, e);
	const t = n.getType();
	let i;
	switch (t) {
		case "Point": {
			i = r0(n);
			break;
		}
		case "LineString": {
			i = t0(n);
			break;
		}
		case "Polygon": {
			i = o0(n, e);
			break;
		}
		case "MultiPoint": {
			i = n0(n);
			break;
		}
		case "MultiLineString": {
			i = i0(n);
			break;
		}
		case "MultiPolygon": {
			i = s0(n, e);
			break;
		}
		case "GeometryCollection": {
			i = e0(n, e);
			break;
		}
		case "Circle": {
			i = {
				type: "GeometryCollection",
				geometries: []
			};
			break;
		}
		default: throw new Error("Unsupported geometry type: " + t);
	}
	return i;
}
function e0(n, e) {
	e = Object.assign({}, e);
	delete e.featureProjection;
	return {
		type: "GeometryCollection",
		geometries: n.getGeometriesArray().map(function(i) {
			return ka(i, e);
		})
	};
}
function t0(n, e) {
	return {
		type: "LineString",
		coordinates: n.getCoordinates()
	};
}
function i0(n, e) {
	return {
		type: "MultiLineString",
		coordinates: n.getCoordinates()
	};
}
function n0(n, e) {
	return {
		type: "MultiPoint",
		coordinates: n.getCoordinates()
	};
}
function s0(n, e) {
	let t;
	if (e) {
		t = e.rightHanded;
	}
	return {
		type: "MultiPolygon",
		coordinates: n.getCoordinates(t)
	};
}
function r0(n, e) {
	return {
		type: "Point",
		coordinates: n.getCoordinates()
	};
}
function o0(n, e) {
	let t;
	if (e) {
		t = e.rightHanded;
	}
	return {
		type: "Polygon",
		coordinates: n.getCoordinates(t)
	};
}
const a0 = {
	Point: u0,
	LineString: d0,
	Polygon: m0,
	MultiPoint: g0,
	MultiLineString: f0,
	MultiPolygon: _0
};
const l0 = {
	Point: p0,
	LineString: y0,
	Polygon: x0,
	MultiPoint: T0,
	MultiLineString: E0,
	MultiPolygon: C0
};
class h0 extends Xy {
	constructor(e) {
		e = e || {};
		super();
		this.geometryName_ = e.geometryName;
	}
	readFeatureFromObject(e, t, i) {
		const s = e, r = Sc(e.geometry, t), o = new sl();
		if (this.geometryName_) {
			o.setGeometryName(this.geometryName_);
		}
		o.setGeometry(r);
		if (e.attributes) {
			o.setProperties(s.attributes, true);
			const a = s.attributes[i];
			if (s.attributes[i] !== "undefined") {
				o.setId(s.attributes[i]);
			}
		}
		return o;
	}
	readFeaturesFromObject(e, t) {
		t = t || {};
		if (e.features) {
			const i = e;
			const s = [];
			const r = e.features;
			for (let o = 0, a = e.features.length; o < a; ++o) s.push(this.readFeatureFromObject(e.features[o], t, e.objectIdFieldName));
			return s;
		}
		return [this.readFeatureFromObject(e, t)];
	}
	readGeometryFromObject(e, t) {
		return Sc(e, t);
	}
	readProjectionFromObject(e) {
		if (e.spatialReference && e.spatialReference.wkid !== "undefined") {
			const i = e.spatialReference.wkid;
			return H("EPSG:" + e.spatialReference.wkid);
		}
		return null;
	}
	writeGeometryObject(e, t) {
		return vc(e, this.adaptOptions(t));
	}
	writeFeatureObject(e, t) {
		t = this.adaptOptions(t);
		const i = {};
		if (!e.hasProperties()) return i.attributes = {}, i;
		const s = e.getProperties(), r = e.getGeometry();
		if (r) {
			i.geometry = vc(r, t);
			const o = t && (t.dataProjection || t.featureProjection);
			if (o) {
				i.geometry.spatialReference = { wkid: Number(H(o).getCode().split(":").pop()) };
			}
			delete s[e.getGeometryName()];
		}
		si(s) ? i.attributes = {} : i.attributes = s;
		return i;
	}
	writeFeaturesObject(e, t) {
		t = this.adaptOptions(t);
		const i = [];
		for (let s = 0, r = e.length; s < r; ++s) i.push(this.writeFeatureObject(e[s], t));
		return { features: i };
	}
}
function Sc(n, e) {
	if (!n) return null;
	let t;
	if (typeof n.x == "number" && typeof n.y == "number") t = "Point";
	else if (n.points) t = "MultiPoint";
	else if (n.paths) n.paths.length === 1 ? t = "LineString" : t = "MultiLineString";
	else if (n.rings) {
		const s = n;
		const r = jn(n);
		const o = c0(n.rings, r);
		o.length === 1 ? (t = "Polygon", n = Object.assign({}, n, { rings: o[0] })) : (t = "MultiPolygon", n = Object.assign({}, n, { rings: o }));
	}
	const i = a0[t];
	return wi(a0[t](n), false, e);
}
function c0(n, e) {
	const t = [], i = [], s = [];
	let r, o;
	for (r = 0, o = n.length; r < o; ++r) {
		t.length = 0;
		Os(t, 0, n[r], e.length);
		lo(t, 0, 0, e.length) ? i.push([n[r]]) : s.push(n[r]);
	}
	for (; s.length;) {
		const a = s.shift();
		let l = false;
		for (r = i.length - 1; r >= 0; r--) {
			const h = i[r][0];
			if (at(new Or(i[r][0]).getExtent(), new Or(a).getExtent())) {
				i[r].push(a);
				l = true;
				break;
			}
		}
		i.push([a.reverse()]);
	}
	return i;
}
function u0(n) {
	let e;
	n.m !== "undefined" && n.z !== "undefined" ? e = new pl([
		n.x,
		n.y,
		n.z,
		n.m
	], "XYZM") : n.z !== "undefined" ? e = new pl([
		n.x,
		n.y,
		n.z
	], "XYZ") : n.m !== "undefined" ? e = new pl([
		n.x,
		n.y,
		n.m
	], "XYM") : e = new pl([n.x, n.y]);
	return e;
}
function d0(n) {
	const e = jn(n);
	return new Nr(n.paths[0], e);
}
function f0(n) {
	const e = jn(n);
	return new kr(n.paths, e);
}
function jn(n) {
	let e = "XY";
	n.hasZ === true && n.hasM === true ? e = "XYZM" : n.hasZ === true ? e = "XYZ" : n.hasM === true && (e = "XYM");
	return e;
}
function g0(n) {
	const e = jn(n);
	return new yl(n.points, e);
}
function _0(n) {
	const e = jn(n);
	return new Gr(n.rings, e);
}
function m0(n) {
	const e = jn(n);
	return new wn(n.rings, e);
}
function p0(n, e) {
	const t = n.getCoordinates();
	let i;
	const s = n.getLayout();
	if (s === "XYZ") i = {
		x: t[0],
		y: t[1],
		z: t[2]
	};
	else if (s === "XYM") i = {
		x: t[0],
		y: t[1],
		m: t[2]
	};
	else if (s === "XYZM") i = {
		x: t[0],
		y: t[1],
		z: t[2],
		m: t[3]
	};
	else if (s === "XY") i = {
		x: t[0],
		y: t[1]
	};
	else throw new Error("Invalid geometry layout");
	return i;
}
function zs(n) {
	const e = n.getLayout();
	return {
		hasZ: e === "XYZ" || e === "XYZM",
		hasM: e === "XYM" || e === "XYZM"
	};
}
function y0(n, e) {
	const t = zs(n);
	return {
		hasZ: t.hasZ,
		hasM: t.hasM,
		paths: [n.getCoordinates()]
	};
}
function x0(n, e) {
	const t = zs(n);
	return {
		hasZ: t.hasZ,
		hasM: t.hasM,
		rings: n.getCoordinates(false)
	};
}
function E0(n, e) {
	const t = zs(n);
	return {
		hasZ: t.hasZ,
		hasM: t.hasM,
		paths: n.getCoordinates()
	};
}
function T0(n, e) {
	const t = zs(n);
	return {
		hasZ: t.hasZ,
		hasM: t.hasM,
		points: n.getCoordinates()
	};
}
function C0(n, e) {
	const t = zs(n), i = n.getCoordinates(false), s = [];
	for (let r = 0; r < i.length; r++) for (let o = i[r].length - 1; o >= 0; o--) s.push(i[r][o]);
	return {
		hasZ: t.hasZ,
		hasM: t.hasM,
		rings: s
	};
}
function vc(n, e) {
	const t = l0[n.getType()];
	return l0[n.getType()](wi(n, true, e), e);
}
function ih(n, e) {
	return Sd(n, e, []).join("");
}
function Sd(n, e, t) {
	if (n.nodeType == Node.CDATA_SECTION_NODE || n.nodeType == Node.TEXT_NODE) e ? t.push(String(n.nodeValue).replace(/(\r\n|\r|\n)/g, "")) : t.push(n.nodeValue);
	else {
		let i;
		for (i = n.firstChild; i; i = i.nextSibling) Sd(i, e, t);
	}
	return t;
}
function R0(n) {
	return "documentElement" in n;
}
function S0(n) {
	return new DOMParser().parseFromString(n, "application/xml");
}
function Ga(n, e) {
	return function(t, i) {
		const s = n.call(e != null ? e : this, t, i);
		if (s !== "undefined") {
			i[i.length - 1].push(s);
		}
	};
}
function Ye(n, e, t) {
	return function(i, s) {
		const r = n.call(t != null ? t : this, i, s);
		if (r !== "undefined") {
			const o = s[s.length - 1];
			const a = e !== "undefined" ? e : i.localName;
			let l;
			a in s[s.length - 1] ? l = s[s.length - 1][a] : (l = [], s[s.length - 1][a] = l);
			l.push(r);
		}
	};
}
function j(n, e, t) {
	return function(i, s) {
		const r = n.call(t != null ? t : this, i, s);
		if (r !== "undefined") {
			const o = s[s.length - 1];
			const a = e !== "undefined" ? e : i.localName;
			s[s.length - 1][a] = r;
		}
	};
}
function he(n, e, t) {
	t = t !== "undefined" ? t : {};
	let i, s;
	for (i = 0, s = n.length; i < s; ++i) t[n[i]] = e;
	return t;
}
function v0(n, e, t, i) {
	let s;
	for (s = e.firstElementChild; s; s = s.nextElementSibling) {
		const r = n[s.namespaceURI];
		if (n[s.namespaceURI] !== "undefined") {
			const o = r[s.localName];
			if (r[s.localName] !== "undefined") {
				r[s.localName].call(i, s, t);
			}
		}
	}
}
function ge(n, e, t, i, s) {
	i.push(n);
	v0(e, t, i, s);
	return i.pop();
}
function w0(n) {
	const e = ih(n, false);
	return P0(e);
}
function P0(n) {
	const e = /^\s*([+\-]?\d*\.?\d+(?:e[+\-]?\d+)?)\s*$/i.exec(n);
	if (e) return parseFloat(e[1]);
}
function Ei(n) {
	const e = ih(n, false);
	return I0(e);
}
function I0(n) {
	const e = /^\s*(\d+)\s*$/.exec(n);
	if (e) return parseInt(e[1], 10);
}
function te(n) {
	return ih(n, false).trim();
}
class F0 extends Ql {
	constructor() {
		super();
	}
	getType() {
		return "text";
	}
	readFeature(e, t) {
		return this.readFeatureFromText(or(e), this.adaptOptions(t));
	}
	readFeatureFromText(e, t) {
		return z();
	}
	readFeatures(e, t) {
		return this.readFeaturesFromText(or(e), this.adaptOptions(t));
	}
	readFeaturesFromText(e, t) {
		return z();
	}
	readGeometry(e, t) {
		return this.readGeometryFromText(or(e), this.adaptOptions(t));
	}
	readGeometryFromText(e, t) {
		return z();
	}
	readProjection(e) {
		return this.readProjectionFromText(or(e));
	}
	readProjectionFromText(e) {
		return this.dataProjection;
	}
	writeFeature(e, t) {
		return this.writeFeatureText(e, this.adaptOptions(t));
	}
	writeFeatureText(e, t) {
		return z();
	}
	writeFeatures(e, t) {
		return this.writeFeaturesText(e, this.adaptOptions(t));
	}
	writeFeaturesText(e, t) {
		return z();
	}
	writeGeometry(e, t) {
		return this.writeGeometryText(e, this.adaptOptions(t));
	}
	writeGeometryText(e, t) {
		return z();
	}
}
function or(n) {
	return typeof n == "string" ? n : "";
}
const Ba = 4294967296;
const wc = 23283064365386963e-26;
const A0 = 12;
const Pc = typeof TextDecoder > "u" ? null : new TextDecoder("utf-8");
const Jo = 0;
const ar = 1;
const es = 2;
const lr = 5;
class M0 {
	constructor(e = new Uint8Array(16)) {
		this.buf = ArrayBuffer.isView(e) ? e : new Uint8Array(e);
		this.dataView = new DataView(this.buf.buffer);
		this.pos = 0;
		this.type = 0;
		this.length = this.buf.length;
	}
	readFields(e, t, i = this.length) {
		for (; this.pos < i;) {
			const s = this.readVarint();
			const r = s >> 3;
			const o = this.pos;
			this.type = s & 7;
			e(r, t, this);
			if (this.pos === this.pos) {
				this.skip(s);
			}
		}
		return t;
	}
	readMessage(e, t) {
		return this.readFields(e, t, this.readVarint() + this.pos);
	}
	readFixed32() {
		const e = this.dataView.getUint32(this.pos, true);
		this.pos += 4;
		return e;
	}
	readSFixed32() {
		const e = this.dataView.getInt32(this.pos, true);
		this.pos += 4;
		return e;
	}
	readFixed64() {
		const e = this.dataView.getUint32(this.pos, true) + this.dataView.getUint32(this.pos + 4, true) * Ba;
		this.pos += 8;
		return e;
	}
	readSFixed64() {
		const e = this.dataView.getUint32(this.pos, true) + this.dataView.getInt32(this.pos + 4, true) * Ba;
		this.pos += 8;
		return e;
	}
	readFloat() {
		const e = this.dataView.getFloat32(this.pos, true);
		this.pos += 4;
		return e;
	}
	readDouble() {
		const e = this.dataView.getFloat64(this.pos, true);
		this.pos += 8;
		return e;
	}
	readVarint(e) {
		const t = this.buf;
		let i, s;
		s = this.buf[this.pos++];
		i = s & 127;
		return s < 128 || (s = this.buf[this.pos++], i |= (s & 127) << 7, s < 128) || (s = this.buf[this.pos++], i |= (s & 127) << 14, s < 128) || (s = this.buf[this.pos++], i |= (s & 127) << 21, s < 128) ? i : (s = this.buf[this.pos], i |= (s & 15) << 28, b0(i, e, this));
	}
	readVarint64() {
		return this.readVarint(true);
	}
	readSVarint() {
		const e = this.readVarint();
		return e % 2 === 1 ? (e + 1) / -2 : e / 2;
	}
	readBoolean() {
		return !!this.readVarint();
	}
	readString() {
		const e = this.readVarint() + this.pos, t = this.pos;
		this.pos = e;
		return e - this.pos >= A0 && Pc ? Pc.decode(this.buf.subarray(this.pos, e)) : V0(this.buf, this.pos, e);
	}
	readBytes() {
		const e = this.readVarint() + this.pos, t = this.buf.subarray(this.pos, e);
		this.pos = e;
		return t;
	}
	readPackedVarint(e = [], t) {
		const i = this.readPackedEnd();
		for (; this.pos < i;) e.push(this.readVarint(t));
		return e;
	}
	readPackedSVarint(e = []) {
		const t = this.readPackedEnd();
		for (; this.pos < t;) e.push(this.readSVarint());
		return e;
	}
	readPackedBoolean(e = []) {
		const t = this.readPackedEnd();
		for (; this.pos < t;) e.push(this.readBoolean());
		return e;
	}
	readPackedFloat(e = []) {
		const t = this.readPackedEnd();
		for (; this.pos < t;) e.push(this.readFloat());
		return e;
	}
	readPackedDouble(e = []) {
		const t = this.readPackedEnd();
		for (; this.pos < t;) e.push(this.readDouble());
		return e;
	}
	readPackedFixed32(e = []) {
		const t = this.readPackedEnd();
		for (; this.pos < t;) e.push(this.readFixed32());
		return e;
	}
	readPackedSFixed32(e = []) {
		const t = this.readPackedEnd();
		for (; this.pos < t;) e.push(this.readSFixed32());
		return e;
	}
	readPackedFixed64(e = []) {
		const t = this.readPackedEnd();
		for (; this.pos < t;) e.push(this.readFixed64());
		return e;
	}
	readPackedSFixed64(e = []) {
		const t = this.readPackedEnd();
		for (; this.pos < t;) e.push(this.readSFixed64());
		return e;
	}
	readPackedEnd() {
		return this.type === es ? this.readVarint() + this.pos : this.pos + 1;
	}
	skip(e) {
		const t = e & 7;
		if (t === Jo) for (; this.buf[this.pos++] > 127;);
		else if (t === es) this.pos = this.readVarint() + this.pos;
		else if (t === lr) this.pos += 4;
		else if (t === ar) this.pos += 8;
		else throw new Error("Unimplemented type: ".concat(t));
	}
	writeTag(e, t) {
		this.writeVarint(e << 3 | t);
	}
	realloc(e) {
		let t = this.length || 16;
		for (; t < this.pos + e;) t *= 2;
		if (t !== this.length) {
			const i = new Uint8Array(t);
			i.set(this.buf);
			this.buf = i;
			this.dataView = new DataView(i.buffer);
			this.length = t;
		}
	}
	finish() {
		this.length = this.pos;
		this.pos = 0;
		return this.buf.subarray(0, this.length);
	}
	writeFixed32(e) {
		this.realloc(4);
		this.dataView.setInt32(this.pos, e, true);
		this.pos += 4;
	}
	writeSFixed32(e) {
		this.realloc(4);
		this.dataView.setInt32(this.pos, e, true);
		this.pos += 4;
	}
	writeFixed64(e) {
		this.realloc(8);
		this.dataView.setInt32(this.pos, e & -1, true);
		this.dataView.setInt32(this.pos + 4, Math.floor(e * wc), true);
		this.pos += 8;
	}
	writeSFixed64(e) {
		this.realloc(8);
		this.dataView.setInt32(this.pos, e & -1, true);
		this.dataView.setInt32(this.pos + 4, Math.floor(e * wc), true);
		this.pos += 8;
	}
	writeVarint(e) {
		e = +e || 0;
		if (e > 268435455 || e < 0) {
			O0(e, this);
			return;
		}
		this.realloc(4);
		this.buf[this.pos++] = e & 127 | (e > 127 ? 128 : 0);
		if (!(e <= 127)) {
			this.buf[this.pos++] = (e >>>= 7) & 127 | (e > 127 ? 128 : 0);
			if (!(e <= 127)) {
				this.buf[this.pos++] = (e >>>= 7) & 127 | (e > 127 ? 128 : 0);
				if (!(e <= 127)) {
					this.buf[this.pos++] = e >>> 7 & 127;
				}
			}
		}
	}
	writeSVarint(e) {
		this.writeVarint(e < 0 ? -e * 2 - 1 : e * 2);
	}
	writeBoolean(e) {
		this.writeVarint(+e);
	}
	writeString(e) {
		e = String(e);
		this.realloc(e.length * 4);
		this.pos++;
		const t = this.pos;
		this.pos = Y0(this.buf, e, this.pos);
		const i = this.pos - this.pos;
		if (i >= 128) {
			Ic(this.pos, i, this);
		}
		this.pos = this.pos - 1;
		this.writeVarint(i);
		this.pos += i;
	}
	writeFloat(e) {
		this.realloc(4);
		this.dataView.setFloat32(this.pos, e, true);
		this.pos += 4;
	}
	writeDouble(e) {
		this.realloc(8);
		this.dataView.setFloat64(this.pos, e, true);
		this.pos += 8;
	}
	writeBytes(e) {
		const t = e.length;
		this.writeVarint(e.length);
		this.realloc(e.length);
		for (let i = 0; i < e.length; i++) this.buf[this.pos++] = e[i];
	}
	writeRawMessage(e, t) {
		this.pos++;
		const i = this.pos;
		e(t, this);
		const s = this.pos - this.pos;
		if (s >= 128) {
			Ic(this.pos, s, this);
		}
		this.pos = this.pos - 1;
		this.writeVarint(s);
		this.pos += s;
	}
	writeMessage(e, t, i) {
		this.writeTag(e, es);
		this.writeRawMessage(t, i);
	}
	writePackedVarint(e, t) {
		if (t.length) {
			this.writeMessage(e, k0, t);
		}
	}
	writePackedSVarint(e, t) {
		if (t.length) {
			this.writeMessage(e, G0, t);
		}
	}
	writePackedBoolean(e, t) {
		if (t.length) {
			this.writeMessage(e, U0, t);
		}
	}
	writePackedFloat(e, t) {
		if (t.length) {
			this.writeMessage(e, B0, t);
		}
	}
	writePackedDouble(e, t) {
		if (t.length) {
			this.writeMessage(e, $0, t);
		}
	}
	writePackedFixed32(e, t) {
		if (t.length) {
			this.writeMessage(e, j0, t);
		}
	}
	writePackedSFixed32(e, t) {
		if (t.length) {
			this.writeMessage(e, z0, t);
		}
	}
	writePackedFixed64(e, t) {
		if (t.length) {
			this.writeMessage(e, X0, t);
		}
	}
	writePackedSFixed64(e, t) {
		if (t.length) {
			this.writeMessage(e, W0, t);
		}
	}
	writeBytesField(e, t) {
		this.writeTag(e, es);
		this.writeBytes(t);
	}
	writeFixed32Field(e, t) {
		this.writeTag(e, lr);
		this.writeFixed32(t);
	}
	writeSFixed32Field(e, t) {
		this.writeTag(e, lr);
		this.writeSFixed32(t);
	}
	writeFixed64Field(e, t) {
		this.writeTag(e, ar);
		this.writeFixed64(t);
	}
	writeSFixed64Field(e, t) {
		this.writeTag(e, ar);
		this.writeSFixed64(t);
	}
	writeVarintField(e, t) {
		this.writeTag(e, Jo);
		this.writeVarint(t);
	}
	writeSVarintField(e, t) {
		this.writeTag(e, Jo);
		this.writeSVarint(t);
	}
	writeStringField(e, t) {
		this.writeTag(e, es);
		this.writeString(t);
	}
	writeFloatField(e, t) {
		this.writeTag(e, lr);
		this.writeFloat(t);
	}
	writeDoubleField(e, t) {
		this.writeTag(e, ar);
		this.writeDouble(t);
	}
	writeBooleanField(e, t) {
		this.writeVarintField(e, +t);
	}
}
function b0(n, e, t) {
	const i = t.buf;
	let s, r;
	r = t.buf[t.pos++];
	s = (r & 112) >> 4;
	if (r < 128 || (r = t.buf[t.pos++], s |= (r & 127) << 3, r < 128) || (r = t.buf[t.pos++], s |= (r & 127) << 10, r < 128) || (r = t.buf[t.pos++], s |= (r & 127) << 17, r < 128) || (r = t.buf[t.pos++], s |= (r & 127) << 24, r < 128) || (r = t.buf[t.pos++], s |= (r & 1) << 31, r < 128)) return sn(n, s, e);
	throw new Error("Expected varint not more than 10 bytes");
}
function sn(n, e, t) {
	return t ? e * 4294967296 + (n >>> 0) : (e >>> 0) * 4294967296 + (n >>> 0);
}
function O0(n, e) {
	let t, i;
	n >= 0 ? (t = n % 4294967296 | 0, i = n / 4294967296 | 0) : (t = ~(-n % 4294967296), i = ~(-n / 4294967296), t ^ 4294967295 ? t = t + 1 | 0 : (t = 0, i = i + 1 | 0));
	if (n >= 0x10000000000000000 || n < -0x10000000000000000) throw new Error("Given varint doesn't fit into 10 bytes");
	e.realloc(10);
	D0(t, i, e);
	N0(i, e);
}
function D0(n, e, t) {
	t.buf[t.pos++] = n & 127 | 128;
	n >>>= 7;
	t.buf[t.pos++] = n & 127 | 128;
	n >>>= 7;
	t.buf[t.pos++] = n & 127 | 128;
	n >>>= 7;
	t.buf[t.pos++] = n & 127 | 128;
	n >>>= 7;
	t.buf[t.pos] = n & 127;
}
function N0(n, e) {
	const t = (n & 7) << 4;
	e.buf[e.pos++] |= t | ((n >>>= 3) ? 128 : 0);
	if (n) {
		e.buf[e.pos++] = n & 127 | ((n >>>= 7) ? 128 : 0);
		if (n) {
			e.buf[e.pos++] = n & 127 | ((n >>>= 7) ? 128 : 0);
			if (n) {
				e.buf[e.pos++] = n & 127 | ((n >>>= 7) ? 128 : 0);
				if (n) {
					e.buf[e.pos++] = n & 127 | ((n >>>= 7) ? 128 : 0);
					if (n) {
						e.buf[e.pos++] = n & 127;
					}
				}
			}
		}
	}
}
function Ic(n, e, t) {
	const i = e <= 16383 ? 1 : e <= 2097151 ? 2 : e <= 268435455 ? 3 : Math.floor(Math.log(e) / (Math.LN2 * 7));
	t.realloc(i);
	for (let s = t.pos - 1; s >= n; s--) t.buf[s + i] = t.buf[s];
}
function k0(n, e) {
	for (let t = 0; t < n.length; t++) e.writeVarint(n[t]);
}
function G0(n, e) {
	for (let t = 0; t < n.length; t++) e.writeSVarint(n[t]);
}
function B0(n, e) {
	for (let t = 0; t < n.length; t++) e.writeFloat(n[t]);
}
function $0(n, e) {
	for (let t = 0; t < n.length; t++) e.writeDouble(n[t]);
}
function U0(n, e) {
	for (let t = 0; t < n.length; t++) e.writeBoolean(n[t]);
}
function j0(n, e) {
	for (let t = 0; t < n.length; t++) e.writeFixed32(n[t]);
}
function z0(n, e) {
	for (let t = 0; t < n.length; t++) e.writeSFixed32(n[t]);
}
function X0(n, e) {
	for (let t = 0; t < n.length; t++) e.writeFixed64(n[t]);
}
function W0(n, e) {
	for (let t = 0; t < n.length; t++) e.writeSFixed64(n[t]);
}
function V0(n, e, t) {
	let i = "", s = e;
	for (; s < t;) {
		const r = n[s];
		let o = null;
		let a = n[s] > 239 ? 4 : n[s] > 223 ? 3 : n[s] > 191 ? 2 : 1;
		if (s + a > t) break;
		let l;
		let h;
		let c;
		a === 1 ? n[s] < 128 && (o = n[s]) : a === 2 ? (l = n[s + 1], (l & 192) === 128 && (o = (n[s] & 31) << 6 | l & 63, o <= 127 && (o = null))) : a === 3 ? (l = n[s + 1], h = n[s + 2], (l & 192) === 128 && (h & 192) === 128 && (o = (n[s] & 15) << 12 | (l & 63) << 6 | h & 63, (o <= 2047 || o >= 55296 && o <= 57343) && (o = null))) : a === 4 && (l = n[s + 1], h = n[s + 2], c = n[s + 3], (l & 192) === 128 && (h & 192) === 128 && (c & 192) === 128 && (o = (n[s] & 15) << 18 | (l & 63) << 12 | (h & 63) << 6 | c & 63, (o <= 65535 || o >= 1114112) && (o = null)));
		o === null ? (o = 65533, a = 1) : o > 65535 && (o -= 65536, i += String.fromCharCode(o >>> 10 & 1023 | 55296), o = 56320 | o & 1023);
		i += String.fromCharCode(o);
		s += a;
	}
	return i;
}
function Y0(n, e, t) {
	for (let i = 0, s, r; i < e.length; i++) {
		s = e.charCodeAt(i);
		if (s > 55295 && s < 57344) if (r) if (s < 56320) {
			n[t++] = 239;
			n[t++] = 191;
			n[t++] = 189;
			r = s;
			continue;
		} else {
			s = r - 55296 << 10 | s - 56320 | 65536;
			r = null;
		}
		else {
			s > 56319 || i + 1 === e.length ? (n[t++] = 239, n[t++] = 191, n[t++] = 189) : r = s;
			continue;
		}
		else r && (n[t++] = 239, n[t++] = 191, n[t++] = 189, r = null);
		s < 128 ? n[t++] = s : (s < 2048 ? n[t++] = s >> 6 | 192 : (s < 65536 ? n[t++] = s >> 12 | 224 : (n[t++] = s >> 18 | 240, n[t++] = s >> 12 & 63 | 128), n[t++] = s >> 6 & 63 | 128), n[t++] = s & 63 | 128);
	}
	return t;
}
class Z0 extends Ql {
	constructor(e) {
		super();
		e = e || {};
		this.dataProjection = new Zf({
			code: "",
			units: "tile-pixels"
		});
		this.featureClass = e.featureClass ? e.featureClass : Pn;
		this.geometryName_ = e.geometryName;
		this.layerName_ = e.layerName ? e.layerName : "layer";
		this.layers_ = e.layers ? e.layers : null;
		this.idProperty_ = e.idProperty;
		this.supportedMediaTypes = ["application/vnd.mapbox-vector-tile", "application/x-protobuf"];
	}
	readRawGeometry_(e, t, i, s) {
		e.pos = t.geometry;
		const r = e.readVarint() + e.pos;
		let o = 1, a = 0, l = 0, h = 0, c = 0, u = 0;
		for (; e.pos < r;) {
			const d = e.readVarint();
			o = d & 7;
			a = d >> 3;
			a--;
			if (o === 1 || o === 2) {
				l += e.readSVarint();
				h += e.readSVarint();
				if (o === 1 && c > u) {
					s.push(c);
					u = c;
				}
				i.push(l, h);
				c += 2;
			} else if (o === 7) c > u && (i.push(i[u], i[u + 1]), c += 2);
			else throw new Error("Invalid command found in the PBF");
		}
		if (c > u) {
			s.push(c);
			u = c;
		}
	}
	createFeature_(e, t, i) {
		const s = t.type;
		if (t.type === 0) return null;
		let r;
		const o = t.properties;
		let a;
		this.idProperty_ ? (a = t.properties[this.idProperty_], delete t.properties[this.idProperty_]) : a = t.id;
		t.properties[this.layerName_] = t.layer.name;
		const l = [], h = [];
		this.readRawGeometry_(e, t, l, h);
		const c = Q0(t.type, h.length);
		if (this.featureClass === Pn) {
			r = new this.featureClass(c, l, h, 2, t.properties, a);
			r.transform(i.dataProjection);
		} else {
			let u;
			if (c == "Polygon") {
				const g = ll(l, h);
				u = g.length > 1 ? new Gr(l, "XY", g) : new wn(l, "XY", h);
			} else u = c === "Point" ? new pl(l, "XY") : c === "LineString" ? new Nr(l, "XY") : c === "MultiPoint" ? new yl(l, "XY") : c === "MultiLineString" ? new kr(l, "XY", h) : null;
			const d = this.featureClass;
			r = new this.featureClass();
			if (this.geometryName_) {
				r.setGeometryName(this.geometryName_);
			}
			const f = wi(u, false, i);
			r.setGeometry(f);
			if (a !== "undefined") {
				r.setId(a);
			}
			r.setProperties(o, true);
		}
		return r;
	}
	getType() {
		return "arraybuffer";
	}
	readFeatures(e, t) {
		const i = this.layers_;
		t = this.adaptOptions(t);
		const s = H(t.dataProjection);
		s.setWorldExtent(t.extent);
		t.dataProjection = s;
		const r = new M0(e), o = r.readFields(K0, {}), a = [];
		for (const l in o) {
			if (i && !i.includes(l)) continue;
			const h = o[l];
			const c = o[l] ? [
				0,
				0,
				o[l].extent,
				o[l].extent
			] : null;
			s.setExtent(c);
			for (let u = 0, d = o[l].length; u < d; ++u) {
				const f = J0(r, h, u);
				const g = this.createFeature_(r, f, t);
				if (g !== null) {
					a.push(g);
				}
			}
		}
		return a;
	}
	readProjection(e) {
		return this.dataProjection;
	}
	setLayers(e) {
		this.layers_ = e;
	}
}
function K0(n, e, t) {}
function H0(n, e, t) {}
function q0(n, e, t) {
	n == 3 ? e.type = t.readVarint() : n == 4 && (e.geometry = t.pos);
}
function J0(n, e, t) {
	n.pos = e.features[t];
	const i = n.readVarint() + n.pos, s = {
		layer: e,
		type: 0,
		properties: {}
	};
	n.readFields(q0, s, i);
	return s;
}
function Q0(n, e) {
	let t;
	n === 1 ? t = e === 1 ? "Point" : "MultiPoint" : n === 2 ? t = e === 1 ? "LineString" : "MultiLineString" : n === 3 && (t = "Polygon");
	return t;
}
class ex {
	read(e) {
		if (!e) return null;
		if (typeof e == "string") {
			const t = S0(e);
			return this.readFromDocument(t);
		}
		return R0(e) ? this.readFromDocument(e) : this.readFromNode(e);
	}
	readFromDocument(e) {
		for (let t = e.firstChild; t; t = t.nextSibling) if (t.nodeType == Node.ELEMENT_NODE) return this.readFromNode(t);
		return null;
	}
	readFromNode(e) {
		z();
	}
}
const tx = "http://www.w3.org/1999/xlink";
function nh(n) {
	return n.getAttributeNS(tx, "href");
}
const Je = [null, "http://www.opengis.net/ows/1.1"];
const ix = he(Je, {
	ServiceIdentification: j(Px),
	ServiceProvider: j(Fx),
	OperationsMetadata: j(vx)
});
class nx extends ex {
	constructor() {
		super();
	}
	readFromNode(e) {
		const t = ge({}, ix, e, []);
		return t || null;
	}
}
const sx = he(Je, {
	DeliveryPoint: j(te),
	City: j(te),
	AdministrativeArea: j(te),
	PostalCode: j(te),
	Country: j(te),
	ElectronicMailAddress: j(te)
});
const rx = he(Je, { Value: Ye(Lx) });
const ox = he(Je, { AllowedValues: j(yx) });
const ax = he(Je, {
	Phone: j(wx),
	Address: j(px)
});
const lx = he(Je, { HTTP: j(Rx) });
const hx = he(Je, {
	Get: Ye(Cx),
	Post: "undefined"
});
const cx = he(Je, { DCP: j(Tx) });
const ux = he(Je, { Operation: Sx });
const dx = he(Je, {
	Voice: j(te),
	Facsimile: j(te)
});
const fx = he(Je, { Constraint: Ye(xx) });
const gx = he(Je, {
	IndividualName: j(te),
	PositionName: j(te),
	ContactInfo: j(Ex)
});
const _x = he(Je, {
	Abstract: j(te),
	AccessConstraints: j(te),
	Fees: j(te),
	Title: j(te),
	ServiceTypeVersion: j(te),
	ServiceType: j(te)
});
const mx = he(Je, {
	ProviderName: j(te),
	ProviderSite: j(nh),
	ServiceContact: j(Ix)
});
function px(n, e) {
	return ge({}, sx, n, e);
}
function yx(n, e) {
	return ge({}, rx, n, e);
}
function xx(n, e) {
	const t = n.getAttribute("name");
	if (t) return ge({ name: t }, ox, n, e);
}
function Ex(n, e) {
	return ge({}, ax, n, e);
}
function Tx(n, e) {
	return ge({}, lx, n, e);
}
function Cx(n, e) {
	const t = nh(n);
	if (t) return ge({ href: t }, fx, n, e);
}
function Rx(n, e) {
	return ge({}, hx, n, e);
}
function Sx(n, e) {
	const t = n.getAttribute("name"), i = ge({}, cx, n, e);
	if (!i) return;
	const s = e[e.length - 1];
	e[e.length - 1][t] = i;
}
function vx(n, e) {
	return ge({}, ux, n, e);
}
function wx(n, e) {
	return ge({}, dx, n, e);
}
function Px(n, e) {
	return ge({}, _x, n, e);
}
function Ix(n, e) {
	return ge({}, gx, n, e);
}
function Fx(n, e) {
	return ge({}, mx, n, e);
}
function Lx(n, e) {
	return te(n);
}
const Mx = {
	POINT: pl,
	LINESTRING: Nr,
	POLYGON: wn,
	MULTIPOINT: yl,
	MULTILINESTRING: kr,
	MULTIPOLYGON: Gr
};
const wd = "EMPTY";
const Pd = "Z";
const Id = "M";
const bx = "ZM";
const ae = {
	START: 0,
	TEXT: 1,
	LEFT_PAREN: 2,
	RIGHT_PAREN: 3,
	NUMBER: 4,
	COMMA: 5,
	EOF: 6
};
const Ox = {
	Point: "POINT",
	LineString: "LINESTRING",
	Polygon: "POLYGON",
	MultiPoint: "MULTIPOINT",
	MultiLineString: "MULTILINESTRING",
	MultiPolygon: "MULTIPOLYGON",
	GeometryCollection: "GEOMETRYCOLLECTION",
	Circle: "CIRCLE"
};
class Dx {
	constructor(e) {
		this.wkt = e;
		this.index_ = -1;
	}
	isAlpha_(e) {
		return e >= "a" && e <= "z" || e >= "A" && e <= "Z";
	}
	isNumeric_(e, t) {
		t = t !== "undefined" ? t : false;
		return e >= "0" && e <= "9" || e == "." && !t;
	}
	isWhiteSpace_(e) {
		return e == " " || e == "	" || e == "\r" || e == "\n";
	}
	nextChar_() {
		return this.wkt.charAt(++this.index_);
	}
	nextToken() {
		const e = this.nextChar_(), t = this.index_;
		let i = e, s;
		if (e == "(") s = ae.LEFT_PAREN;
		else if (e == ",") s = ae.COMMA;
		else if (e == ")") s = ae.RIGHT_PAREN;
		else if (this.isNumeric_(e) || e == "-") {
			s = ae.NUMBER;
			i = this.readNumber_();
		} else if (this.isAlpha_(e)) {
			s = ae.TEXT;
			i = this.readText_();
		} else {
			if (this.isWhiteSpace_(e)) return this.nextToken();
			if (e === "") s = ae.EOF;
			else throw new Error("Unexpected character: " + e);
		}
		return {
			position: this.index_,
			value: i,
			type: s
		};
	}
	readNumber_() {
		let e;
		const t = this.index_;
		let i = false, s = false;
		do {
			e == "." ? i = true : (e == "e" || e == "E") && (s = true);
			e = this.nextChar_();
		} while (this.isNumeric_(e, i) || !s && (e == "e" || e == "E") || s && (e == "-" || e == "+"));
		return parseFloat(this.wkt.substring(this.index_, this.index_--));
	}
	readText_() {
		let e;
		const t = this.index_;
		do
			e = this.nextChar_();
		while (this.isAlpha_(e));
		return this.wkt.substring(this.index_, this.index_--).toUpperCase();
	}
}
class Nx {
	constructor(e) {
		this.lexer_ = e;
		this.token_ = {
			position: 0,
			type: ae.START
		};
		this.layout_ = "XY";
	}
	consume_() {
		this.token_ = this.lexer_.nextToken();
	}
	isTokenType(e) {
		return this.token_.type == e;
	}
	match(e) {
		const t = this.isTokenType(e);
		if (t) {
			this.consume_();
		}
		return t;
	}
	parse() {
		this.consume_();
		return this.parseGeometry_();
	}
	parseGeometryLayout_() {
		let e = "XY";
		const t = this.token_;
		if (this.isTokenType(ae.TEXT)) {
			const i = t.value;
			t.value === Pd ? e = "XYZ" : t.value === Id ? e = "XYM" : t.value === bx && (e = "XYZM");
		}
		return e;
	}
	parseGeometryCollectionText_() {
		if (this.match(ae.LEFT_PAREN)) {
			const e = [];
			do
				e.push(this.parseGeometry_());
			while (this.match(ae.COMMA));
			if (this.match(ae.RIGHT_PAREN)) return e;
		}
		throw new Error(this.formatErrorMessage_());
	}
	parsePointText_() {
		if (this.match(ae.LEFT_PAREN)) {
			const e = this.parsePoint_();
			if (this.match(ae.RIGHT_PAREN)) return e;
		}
		throw new Error(this.formatErrorMessage_());
	}
	parseLineStringText_() {
		if (this.match(ae.LEFT_PAREN)) {
			const e = this.parsePointList_();
			if (this.match(ae.RIGHT_PAREN)) return e;
		}
		throw new Error(this.formatErrorMessage_());
	}
	parsePolygonText_() {
		if (this.match(ae.LEFT_PAREN)) {
			const e = this.parseLineStringTextList_();
			if (this.match(ae.RIGHT_PAREN)) return e;
		}
		throw new Error(this.formatErrorMessage_());
	}
	parseMultiPointText_() {
		if (this.match(ae.LEFT_PAREN)) {
			let e;
			this.token_.type == ae.LEFT_PAREN ? e = this.parsePointTextList_() : e = this.parsePointList_();
			if (this.match(ae.RIGHT_PAREN)) return e;
		}
		throw new Error(this.formatErrorMessage_());
	}
	parseMultiLineStringText_() {
		if (this.match(ae.LEFT_PAREN)) {
			const e = this.parseLineStringTextList_();
			if (this.match(ae.RIGHT_PAREN)) return e;
		}
		throw new Error(this.formatErrorMessage_());
	}
	parseMultiPolygonText_() {
		if (this.match(ae.LEFT_PAREN)) {
			const e = this.parsePolygonTextList_();
			if (this.match(ae.RIGHT_PAREN)) return e;
		}
		throw new Error(this.formatErrorMessage_());
	}
	parsePoint_() {
		const e = [], t = this.layout_.length;
		for (let i = 0; i < this.layout_.length; ++i) {
			const s = this.token_;
			if (this.match(ae.NUMBER)) e.push(this.token_.value);
			else break;
		}
		if (e.length == this.layout_.length) return e;
		throw new Error(this.formatErrorMessage_());
	}
	parsePointList_() {
		const e = [this.parsePoint_()];
		for (; this.match(ae.COMMA);) e.push(this.parsePoint_());
		return e;
	}
	parsePointTextList_() {
		const e = [this.parsePointText_()];
		for (; this.match(ae.COMMA);) e.push(this.parsePointText_());
		return e;
	}
	parseLineStringTextList_() {
		const e = [this.parseLineStringText_()];
		for (; this.match(ae.COMMA);) e.push(this.parseLineStringText_());
		return e;
	}
	parsePolygonTextList_() {
		const e = [this.parsePolygonText_()];
		for (; this.match(ae.COMMA);) e.push(this.parsePolygonText_());
		return e;
	}
	isEmptyGeometry_() {
		const e = this.isTokenType(ae.TEXT) && this.token_.value == wd;
		if (e) {
			this.consume_();
		}
		return e;
	}
	formatErrorMessage_() {
		return "Unexpected `" + this.token_.value + "` at position " + this.token_.position + " in `" + this.lexer_.wkt + "`";
	}
	parseGeometry_() {
		const e = this.token_;
		if (this.match(ae.TEXT)) {
			const t = e.value;
			this.layout_ = this.parseGeometryLayout_();
			const i = this.isEmptyGeometry_();
			if (e.value == "GEOMETRYCOLLECTION") {
				if (i) return new br([]);
				const o = this.parseGeometryCollectionText_();
				return new br(o);
			}
			const s = Mx[e.value];
			if (!Mx[e.value]) throw new Error("Invalid geometry type: " + e.value);
			let r;
			if (i) e.value == "POINT" ? r = [NaN, NaN] : r = [];
			else switch (e.value) {
				case "POINT": {
					r = this.parsePointText_();
					break;
				}
				case "LINESTRING": {
					r = this.parseLineStringText_();
					break;
				}
				case "POLYGON": {
					r = this.parsePolygonText_();
					break;
				}
				case "MULTIPOINT": {
					r = this.parseMultiPointText_();
					break;
				}
				case "MULTILINESTRING": {
					r = this.parseMultiLineStringText_();
					break;
				}
				case "MULTIPOLYGON": {
					r = this.parseMultiPolygonText_();
					break;
				}
			}
			return new Mx[e.value](r, this.layout_);
		}
		throw new Error(this.formatErrorMessage_());
	}
}
class kx extends F0 {
	constructor(e) {
		super();
		e = e || {};
		this.splitCollection_ = e.splitCollection !== "undefined" ? e.splitCollection : false;
	}
	parse_(e) {
		const t = new Dx(e);
		return new Nx(t).parse();
	}
	readFeatureFromText(e, t) {
		const i = this.readGeometryFromText(e, t), s = new sl();
		s.setGeometry(i);
		return s;
	}
	readFeaturesFromText(e, t) {
		let i = [];
		const s = this.readGeometryFromText(e, t);
		this.splitCollection_ && s.getType() == "GeometryCollection" ? i = s.getGeometriesArray() : i = [s];
		const r = [];
		for (let o = 0, a = i.length; o < a; ++o) {
			const l = new sl();
			l.setGeometry(i[o]);
			r.push(l);
		}
		return r;
	}
	readGeometryFromText(e, t) {
		const i = this.parse_(e);
		return wi(i, false, t);
	}
	writeFeatureText(e, t) {
		const i = e.getGeometry();
		return i ? this.writeGeometryText(i, t) : "";
	}
	writeFeaturesText(e, t) {
		if (e.length == 1) return this.writeFeatureText(e[0], t);
		const i = [];
		for (let r = 0, o = e.length; r < o; ++r) i.push(e[r].getGeometry());
		const s = new br(i);
		return this.writeGeometryText(s, t);
	}
	writeGeometryText(e, t) {
		return Ad(wi(e, true, t));
	}
}
function Fd(n) {
	const e = n.getCoordinates();
	return e.length === 0 ? "" : e.join(" ");
}
function Gx(n) {
	const e = [], t = n.getPoints();
	for (let i = 0, s = t.length; i < s; ++i) e.push("(" + Fd(t[i]) + ")");
	return e.join(",");
}
function Bx(n) {
	const e = [], t = n.getGeometries();
	for (let i = 0, s = t.length; i < s; ++i) e.push(Ad(t[i]));
	return e.join(",");
}
function sh(n) {
	const e = n.getCoordinates(), t = [];
	for (let i = 0, s = e.length; i < s; ++i) t.push(e[i].join(" "));
	return t.join(",");
}
function $x(n) {
	const e = [], t = n.getLineStrings();
	for (let i = 0, s = t.length; i < s; ++i) e.push("(" + sh(t[i]) + ")");
	return e.join(",");
}
function Ld(n) {
	const e = [], t = n.getLinearRings();
	for (let i = 0, s = t.length; i < s; ++i) e.push("(" + sh(t[i]) + ")");
	return e.join(",");
}
function Ux(n) {
	const e = [], t = n.getPolygons();
	for (let i = 0, s = t.length; i < s; ++i) e.push("(" + Ld(t[i]) + ")");
	return e.join(",");
}
function jx(n) {
	const e = n.getLayout();
	let t = "";
	if (e === "XYZ" || e === "XYZM") {
		t += Pd;
	}
	if (e === "XYM" || e === "XYZM") {
		t += Id;
	}
	return t;
}
const zx = {
	Point: Fd,
	LineString: sh,
	Polygon: Ld,
	MultiPoint: Gx,
	MultiLineString: $x,
	MultiPolygon: Ux,
	GeometryCollection: Bx
};
function Ad(n) {
	const e = n.getType(), t = zx[e], i = zx[e](n);
	let s = Ox[e];
	if (typeof n.getFlatCoordinates == "function") {
		const r = jx(n);
		if (r.length > 0) {
			s += " " + r;
		}
	}
	return i.length === 0 ? s + " " + wd : s + "(" + i + ")";
}
const Gt = [null, "http://www.opengis.net/wmts/1.0"];
const zn = [null, "http://www.opengis.net/ows/1.1"];
const Xx = he(Gt, { Contents: j(tE) });
class Wx extends ex {
	constructor() {
		super();
		this.owsParser_ = new nx();
	}
	readFromNode(e) {
		let t = e.getAttribute("version");
		if (t) {
			t = t.trim();
		}
		let i = this.owsParser_.readFromNode(e);
		return i ? (i.version = t, i = ge(i, Xx, e, []), i || null) : null;
	}
}
const Vx = he(Gt, {
	Layer: Ye(iE),
	TileMatrixSet: Ye(nE)
});
const Yx = he(Gt, {
	Style: Ye(sE),
	Format: Ye(te),
	TileMatrixSetLink: Ye(rE),
	Dimension: Ye(oE),
	ResourceURL: Ye(aE)
}, he(zn, {
	Title: j(te),
	Abstract: j(te),
	WGS84BoundingBox: j(bd),
	BoundingBox: Ye(lE),
	Identifier: j(te)
}));
const Zx = he(Gt, { LegendURL: Ye(hE) }, he(zn, {
	Title: j(te),
	Identifier: j(te)
}));
const Kx = he(Gt, {
	TileMatrixSet: j(te),
	TileMatrixSetLimits: j(uE)
});
const Hx = he(Gt, { TileMatrixLimits: Ga(dE) });
const qx = he(Gt, {
	TileMatrix: j(te),
	MinTileRow: j(Ei),
	MaxTileRow: j(Ei),
	MinTileCol: j(Ei),
	MaxTileCol: j(Ei)
});
const Jx = he(Gt, {
	Default: j(te),
	Value: Ye(te)
}, he(zn, { Identifier: j(te) }));
const Md = he(zn, {
	LowerCorner: Ga($a),
	UpperCorner: Ga($a)
});
const Qx = he(Gt, {
	WellKnownScaleSet: j(te),
	TileMatrix: Ye(cE)
}, he(zn, {
	SupportedCRS: j(te),
	Identifier: j(te),
	BoundingBox: j(bd)
}));
const eE = he(Gt, {
	TopLeftCorner: j($a),
	ScaleDenominator: j(w0),
	TileWidth: j(Ei),
	TileHeight: j(Ei),
	MatrixWidth: j(Ei),
	MatrixHeight: j(Ei)
}, he(zn, { Identifier: j(te) }));
function tE(n, e) {
	return ge({}, Vx, n, e);
}
function iE(n, e) {
	return ge({}, Yx, n, e);
}
function nE(n, e) {
	return ge({}, Qx, n, e);
}
function sE(n, e) {
	const t = ge({}, Zx, n, e);
	if (!t) return;
	const i = n.getAttribute("isDefault") === "true";
	t.isDefault = i;
	return t;
}
function rE(n, e) {
	return ge({}, Kx, n, e);
}
function oE(n, e) {
	return ge({}, Jx, n, e);
}
function aE(n, e) {
	const t = n.getAttribute("format"), i = n.getAttribute("template"), s = n.getAttribute("resourceType"), r = {};
	if (t) {
		r.format = t;
	}
	if (i) {
		r.template = i;
	}
	if (s) {
		r.resourceType = s;
	}
	return r;
}
function bd(n, e) {
	const t = ge([], Md, n, e);
	if (t.length == 2) return ye(t);
}
function lE(n, e) {
	const t = n.getAttribute("crs"), i = ge([], Md, n, e);
	if (i.length == 2) return {
		extent: ye(i),
		crs: t
	};
}
function hE(n, e) {
	const t = {};
	t.format = n.getAttribute("format");
	t.href = nh(n);
	return t;
}
function $a(n, e) {
	const t = te(n).split(/\s+/);
	if (!t || t.length != 2) return;
	const i = +t[0], s = +t[1];
	if (!(isNaN(i) || isNaN(s))) return [i, s];
}
function cE(n, e) {
	return ge({}, eE, n, e);
}
function uE(n, e) {
	return ge([], Hx, n, e);
}
function dE(n, e) {
	return ge({}, qx, n, e);
}
function Ua(n) {
	return n instanceof Image || n instanceof HTMLCanvasElement || n instanceof HTMLVideoElement || n instanceof ImageBitmap ? n : null;
}
const fE = new Error("disposed");
const gE = [256, 256];
class _E extends Lp {
	constructor(e) {
		const t = M.IDLE;
		super(e.tileCoord, M.IDLE, {
			transition: e.transition,
			interpolate: e.interpolate
		});
		this.loader_ = e.loader;
		this.data_ = null;
		this.error_ = null;
		this.size_ = e.size || null;
		this.controller_ = e.controller || null;
	}
	getSize() {
		if (this.size_) return this.size_;
		const e = Ua(this.data_);
		return e ? [e.width, e.height] : gE;
	}
	getData() {
		return this.data_;
	}
	getError() {
		return this.error_;
	}
	load() {
		if (this.state !== M.IDLE && this.state !== M.ERROR) return;
		this.state = M.LOADING;
		this.changed();
		const e = this;
		this.loader_().then(function(t) {
			e.data_ = t;
			e.state = M.LOADED;
			e.changed();
		}).catch(function(t) {
			e.error_ = t;
			e.state = M.ERROR;
			e.changed();
		});
	}
	disposeInternal() {
		if (this.controller_) {
			this.controller_.abort(fE);
			this.controller_ = null;
		}
		super.disposeInternal();
	}
}
let Qo;
const Cn = [];
function Lc(n, e, t, i, s) {
	n.beginPath();
	n.moveTo(0, 0);
	n.lineTo(e, t);
	n.lineTo(i, s);
	n.closePath();
	n.save();
	n.clip();
	n.fillRect(0, 0, Math.max(e, i) + 1, Math.max(t, s));
	n.restore();
}
function ea(n, e) {
	return Math.abs(n[e * 4] - 210) > 2 || Math.abs(n[e * 4 + 3] - 191.25) > 2;
}
function mE() {
	if (Qo === "undefined") {
		const n = Re(6, 6, Cn);
		n.globalCompositeOperation = "lighter";
		n.fillStyle = "rgba(210, 0, 0, 0.75)";
		Lc(n, 4, 5, 4, 0);
		Lc(n, 4, 5, 0, 5);
		const e = n.getImageData(0, 0, 3, 3).data;
		Qo = ea(n.getImageData(0, 0, 3, 3).data, 0) || ea(n.getImageData(0, 0, 3, 3).data, 4) || ea(n.getImageData(0, 0, 3, 3).data, 8);
		Ns(n);
		Cn.push(n.canvas);
	}
	return Qo;
}
function ws(n, e, t, i) {
	const s = On(t, e, n);
	let r = jh(e, i, t);
	const o = e.getMetersPerUnit();
	if (o !== "undefined") {
		r *= o;
	}
	const a = n.getMetersPerUnit();
	if (a !== "undefined") {
		r /= a;
	}
	const l = n.getExtent();
	if (!l || Ui(l, s)) {
		const h = jh(n, r, s) / r;
		if (isFinite(h) && h > 0) {
			r /= h;
		}
	}
	return r;
}
function pE(n, e, t, i) {
	const s = Ot(t);
	let r = ws(n, e, s, i);
	if (!isFinite(r) || r <= 0) {
		Ka(t, function(o) {
			r = ws(n, e, o, i);
			return isFinite(r) && r > 0;
		});
	}
	return r;
}
function Od(n, e, t, i, s, r, o, a, l, h, c, u, d, f) {
	const g = Re(Math.round(t * n), Math.round(t * e), Cn);
	if (!u) {
		g.imageSmoothingEnabled = false;
	}
	if (l.length === 0) return g.canvas;
	g.scale(t, t);
	function m(T) {
		return Math.round(T * t) / t;
	}
	g.globalCompositeOperation = "lighter";
	const _ = je();
	l.forEach(function(T, v, P) {
		Hc(_, T.extent);
	});
	let p;
	const y = t / i, E = (u ? 1 : 1 + Math.pow(2, -24)) / y;
	if (!d || l.length !== 1 || h !== 0) {
		p = Re(Math.round(J(_) * y), Math.round(Ce(_) * y), Cn);
		if (!u) {
			p.imageSmoothingEnabled = false;
		}
		if (s && f) {
			const T = (s[0] - _[0]) * y;
			const v = -(s[3] - _[3]) * y;
			const P = J(s) * y;
			const S = Ce(s) * y;
			p.rect(T, v, P, S);
			p.clip();
		}
		l.forEach(function(T, v, P) {
			if (T.image.width > 0 && T.image.height > 0) {
				if (T.clipExtent) {
					p.save();
					const L = (T.clipExtent[0] - _[0]) * y;
					const A = -(T.clipExtent[3] - _[3]) * y;
					const W = J(T.clipExtent) * y;
					const w = Ce(T.clipExtent) * y;
					p.rect(u ? L : Math.round(L), u ? A : Math.round(A), u ? W : Math.round(L + W) - Math.round(L), u ? w : Math.round(A + w) - Math.round(A));
					p.clip();
				}
				const S = (T.extent[0] - _[0]) * y;
				const R = -(T.extent[3] - _[3]) * y;
				const I = J(T.extent) * y;
				const N = Ce(T.extent) * y;
				p.drawImage(T.image, h, h, T.image.width - 2 * h, T.image.height - 2 * h, u ? S : Math.round(S), u ? R : Math.round(R), u ? I : Math.round(S + I) - Math.round(S), u ? N : Math.round(R + N) - Math.round(R));
				if (T.clipExtent) {
					p.restore();
				}
			}
		});
	}
	const x = ri(o);
	a.getTriangles().forEach(function(T, v, P) {
		const S = T.source, R = T.target;
		let I = T.source[0][0], N = T.source[0][1], L = T.source[1][0], A = T.source[1][1], W = T.source[2][0], w = T.source[2][1];
		const b = m((T.target[0][0] - x[0]) / r), D = m(-(T.target[0][1] - x[1]) / r), k = m((T.target[1][0] - x[0]) / r), B = m(-(T.target[1][1] - x[1]) / r), q = m((T.target[2][0] - x[0]) / r), ie = m(-(T.target[2][1] - x[1]) / r), re = I, ce = N;
		I = 0;
		N = 0;
		L -= I;
		A -= N;
		W -= I;
		w -= N;
		const Fe = [
			[
				L,
				A,
				0,
				0,
				k - b
			],
			[
				W,
				w,
				0,
				0,
				q - b
			],
			[
				0,
				0,
				L,
				A,
				B - D
			],
			[
				0,
				0,
				W,
				w,
				ie - D
			]
		], ve = Uf(Fe);
		if (!ve) return;
		g.save();
		g.beginPath();
		if (mE() || !u) {
			g.moveTo(k, B);
			const F = 4;
			const ai = b - k;
			const it = D - B;
			for (let Xe = 0; Xe < 4; Xe++) {
				g.lineTo(k + m((Xe + 1) * ai / 4), B + m(Xe * it / 3));
				if (Xe != 3) {
					g.lineTo(k + m((Xe + 1) * ai / 4), B + m((Xe + 1) * it / 3));
				}
			}
			g.lineTo(q, ie);
		} else {
			g.moveTo(k, B);
			g.lineTo(b, D);
			g.lineTo(q, ie);
		}
		g.clip();
		g.transform(ve[0], ve[2], ve[1], ve[3], b, D);
		g.translate(_[0] - I, _[3] - N);
		let ze;
		if (p) {
			ze = p.canvas;
			g.scale(E, -E);
		} else {
			const F = l[0];
			const ai = l[0].extent;
			ze = l[0].image;
			g.scale(J(l[0].extent) / ze.width, -Ce(l[0].extent) / ze.height);
		}
		g.drawImage(ze, 0, 0);
		g.restore();
	});
	if (p) {
		Ns(p);
		Cn.push(p.canvas);
	}
	if (c) {
		g.save();
		g.globalCompositeOperation = "source-over";
		g.strokeStyle = "black";
		g.lineWidth = 1;
		a.getTriangles().forEach(function(T, v, P) {
			const S = T.target, R = (T.target[0][0] - x[0]) / r, I = -(T.target[0][1] - x[1]) / r, N = (T.target[1][0] - x[0]) / r, L = -(T.target[1][1] - x[1]) / r, A = (T.target[2][0] - x[0]) / r, W = -(T.target[2][1] - x[1]) / r;
			g.beginPath();
			g.moveTo(N, L);
			g.lineTo(R, I);
			g.lineTo(A, W);
			g.closePath();
			g.stroke();
		});
		g.restore();
	}
	return g.canvas;
}
const yE = 10;
const Ac = .25;
class xE {
	constructor(e, t, i, s, r, o, a) {
		this.sourceProj_ = e;
		this.targetProj_ = t;
		let l = {};
		const h = a ? Rg((E) => xe(a, On(E, this.targetProj_, this.sourceProj_))) : ji(this.targetProj_, this.sourceProj_);
		this.transformInv_ = function(E) {
			const x = E[0] + "/" + E[1];
			if (!l[x]) {
				l[x] = h(E);
			}
			return l[x];
		};
		this.maxSourceExtent_ = s;
		this.errorThresholdSquared_ = r * r;
		this.triangles_ = [];
		this.wrapsXInSource_ = false;
		this.canWrapXInSource_ = this.sourceProj_.canWrapX() && !!s && !!this.sourceProj_.getExtent() && J(s) >= J(this.sourceProj_.getExtent());
		this.sourceWorldWidth_ = this.sourceProj_.getExtent() ? J(this.sourceProj_.getExtent()) : null;
		this.targetWorldWidth_ = this.targetProj_.getExtent() ? J(this.targetProj_.getExtent()) : null;
		const c = ri(i), u = Ms(i), d = As(i), f = Sn(i), g = this.transformInv_(c), m = this.transformInv_(u), _ = this.transformInv_(d), p = this.transformInv_(f), y = yE + (o ? Math.max(0, Math.ceil(Math.log2(ya(i) / (o * o * 256 * 256)))) : 0);
		this.addQuad_(c, u, d, f, g, m, _, p, y);
		if (this.wrapsXInSource_) {
			let E = null;
			this.triangles_.forEach(function(x, T, v) {
				E = Math.min(E, x.source[0][0], x.source[1][0], x.source[2][0]);
			});
			this.triangles_.forEach((x) => {
				if (Math.max(x.source[0][0], x.source[1][0], x.source[2][0]) - E > this.sourceWorldWidth_ / 2) {
					const T = [
						[x.source[0][0], x.source[0][1]],
						[x.source[1][0], x.source[1][1]],
						[x.source[2][0], x.source[2][1]]
					];
					if (T[0][0] - E > this.sourceWorldWidth_ / 2) {
						T[0][0] -= this.sourceWorldWidth_;
					}
					if (T[1][0] - E > this.sourceWorldWidth_ / 2) {
						T[1][0] -= this.sourceWorldWidth_;
					}
					if (T[2][0] - E > this.sourceWorldWidth_ / 2) {
						T[2][0] -= this.sourceWorldWidth_;
					}
					const v = Math.min(T[0][0], T[1][0], T[2][0]);
					if (Math.max(T[0][0], T[1][0], T[2][0]) - v < this.sourceWorldWidth_ / 2) {
						x.source = T;
					}
				}
			});
		}
		l = {};
	}
	addTriangle_(e, t, i, s, r, o) {
		this.triangles_.push({
			source: [
				s,
				r,
				o
			],
			target: [
				e,
				t,
				i
			]
		});
	}
	addQuad_(e, t, i, s, r, o, a, l, h) {
		const c = ye([
			r,
			o,
			a,
			l
		]), u = this.sourceWorldWidth_ ? J(c) / this.sourceWorldWidth_ : null, d = this.sourceWorldWidth_, f = this.sourceProj_.canWrapX() && u > .5 && u < 1;
		let g = false;
		if (h > 0) {
			if (this.targetProj_.isGlobal() && this.targetWorldWidth_) {
				const _ = ye([
					e,
					t,
					i,
					s
				]);
				g = J(_) / this.targetWorldWidth_ > Ac || g;
			}
			if (!f && this.sourceProj_.isGlobal() && u) {
				g = u > Ac || g;
			}
		}
		if (!g && this.maxSourceExtent_ && isFinite(c[0]) && isFinite(c[1]) && isFinite(c[2]) && isFinite(c[3]) && !me(c, this.maxSourceExtent_)) return;
		let m = 0;
		if (!g && (!isFinite(r[0]) || !isFinite(r[1]) || !isFinite(o[0]) || !isFinite(o[1]) || !isFinite(a[0]) || !isFinite(a[1]) || !isFinite(l[0]) || !isFinite(l[1]))) {
			if (h > 0) g = true;
			else if (m = (!isFinite(r[0]) || !isFinite(r[1]) ? 8 : 0) + (!isFinite(o[0]) || !isFinite(o[1]) ? 4 : 0) + (!isFinite(a[0]) || !isFinite(a[1]) ? 2 : 0) + (!isFinite(l[0]) || !isFinite(l[1]) ? 1 : 0), m != 1 && m != 2 && m != 4 && m != 8) return;
		}
		if (h > 0) {
			if (!g) {
				const _ = [(e[0] + i[0]) / 2, (e[1] + i[1]) / 2];
				const p = this.transformInv_(_);
				let y;
				f ? y = (Qt(r[0], d) + Qt(a[0], d)) / 2 - Qt(p[0], d) : y = (r[0] + a[0]) / 2 - p[0];
				const E = (r[1] + a[1]) / 2 - p[1];
				g = y * y + E * E > this.errorThresholdSquared_;
			}
			if (g) {
				if (Math.abs(e[0] - i[0]) <= Math.abs(e[1] - i[1])) {
					const _ = [(t[0] + i[0]) / 2, (t[1] + i[1]) / 2];
					const p = this.transformInv_(_);
					const y = [(s[0] + e[0]) / 2, (s[1] + e[1]) / 2];
					const E = this.transformInv_(y);
					this.addQuad_(e, t, _, y, r, o, p, E, h - 1);
					this.addQuad_(y, _, i, s, E, p, a, l, h - 1);
				} else {
					const _ = [(e[0] + t[0]) / 2, (e[1] + t[1]) / 2];
					const p = this.transformInv_(_);
					const y = [(i[0] + s[0]) / 2, (i[1] + s[1]) / 2];
					const E = this.transformInv_(y);
					this.addQuad_(e, _, y, s, r, p, E, l, h - 1);
					this.addQuad_(_, t, i, y, p, o, a, E, h - 1);
				}
				return;
			}
		}
		if (f) {
			if (!this.canWrapXInSource_) return;
			this.wrapsXInSource_ = true;
		}
		if (!(m & 11)) {
			this.addTriangle_(e, i, s, r, a, l);
		}
		if (!(m & 14)) {
			this.addTriangle_(e, i, t, r, a, o);
		}
	}
	calculateSourceExtent() {
		const e = je();
		this.triangles_.forEach(function(t, i, s) {
			const r = t.source;
			os(e, t.source[0]);
			os(e, t.source[1]);
			os(e, t.source[2]);
		});
		return e;
	}
	getTriangles() {
		return this.triangles_;
	}
}
const Nd = .5;
class EE extends Lp {
	constructor(e, t, i, s, r, o, a, l, h, c, u, d) {
		super(r, M.IDLE, d);
		this.renderEdges_ = u !== "undefined" ? u : false;
		this.pixelRatio_ = a;
		this.gutter_ = l;
		this.canvas_ = null;
		this.sourceTileGrid_ = t;
		this.targetTileGrid_ = s;
		this.wrappedTileCoord_ = o || r;
		this.sourceTiles_ = [];
		this.sourcesListenerKeys_ = null;
		this.sourceZ_ = 0;
		this.clipExtent_ = e.canWrapX() ? e.getExtent() : "undefined";
		const f = s.getTileCoordExtent(this.wrappedTileCoord_), g = this.targetTileGrid_.getExtent();
		let m = this.sourceTileGrid_.getExtent();
		const _ = g ? Et(f, g) : f;
		if (ya(_) === 0) {
			this.state = M.EMPTY;
			return;
		}
		const p = e.getExtent();
		if (p) {
			m ? m = Et(m, p) : m = p;
		}
		const y = s.getResolution(this.wrappedTileCoord_[0]), E = pE(e, i, _, y);
		if (!isFinite(E) || E <= 0) {
			this.state = M.EMPTY;
			return;
		}
		const x = c !== "undefined" ? c : Nd;
		this.triangulation_ = new xE(e, i, _, m, E * x, y);
		if (this.triangulation_.getTriangles().length === 0) {
			this.state = M.EMPTY;
			return;
		}
		this.sourceZ_ = t.getZForResolution(E);
		let T = this.triangulation_.calculateSourceExtent();
		if (m) {
			e.canWrapX() ? (T[1] = fe(T[1], m[1], m[3]), T[3] = fe(T[3], m[1], m[3])) : T = Et(T, m);
		}
		if (!ya(T)) this.state = M.EMPTY;
		else {
			let v = 0;
			let P = 0;
			if (e.canWrapX()) {
				v = J(p);
				P = Math.floor((T[0] - p[0]) / v);
			}
			eu(T.slice(), e, true).forEach((R) => {
				const I = t.getTileRangeForExtentAndZ(R, this.sourceZ_);
				for (let N = I.minX; N <= I.maxX; N++) for (let L = I.minY; L <= I.maxY; L++) {
					const A = P * v;
					this.sourceTiles_.push({
						getTile: () => h(this.sourceZ_, N, L, a),
						offset: A
					});
				}
				++P;
			});
			if (this.sourceTiles_.length === 0) {
				this.state = M.EMPTY;
			}
		}
	}
	getImage() {
		return this.canvas_;
	}
	reproject_() {
		const e = [];
		this.sourceTiles_.forEach((t) => {
			var s;
			const i = t.tile;
			if (t.tile && t.tile.getState() == M.LOADED) {
				const r = this.sourceTileGrid_.getTileCoordExtent(i.tileCoord);
				r[0] += t.offset;
				r[2] += t.offset;
				const o = (s = this.clipExtent_) == null ? "undefined" : s.slice();
				if (o) {
					o[0] += t.offset;
					o[2] += t.offset;
				}
				e.push({
					extent: r,
					clipExtent: o,
					image: i.getImage()
				});
			}
		});
		this.sourceTiles_.length = 0;
		if (e.length === 0) this.state = M.ERROR;
		else {
			const t = this.wrappedTileCoord_[0];
			const i = this.targetTileGrid_.getTileSize(this.wrappedTileCoord_[0]);
			const s = typeof i == "number" ? i : i[0];
			const r = typeof i == "number" ? i : i[1];
			const o = this.targetTileGrid_.getResolution(this.wrappedTileCoord_[0]);
			const a = this.sourceTileGrid_.getResolution(this.sourceZ_);
			const l = this.targetTileGrid_.getTileCoordExtent(this.wrappedTileCoord_);
			this.canvas_ = Od(s, r, this.pixelRatio_, a, this.sourceTileGrid_.getExtent(), o, l, this.triangulation_, e, this.gutter_, this.renderEdges_, this.interpolate);
			this.state = M.LOADED;
		}
		this.changed();
	}
	load() {
		for (const e of this.sourceTiles_) e.tile = e.getTile();
		if (this.state == M.IDLE) {
			this.state = M.LOADING;
			this.changed();
			let e = 0;
			this.sourcesListenerKeys_ = [];
			this.sourceTiles_.forEach(({ tile: t }) => {
				const i = t.getState();
				if (i == M.IDLE || i == M.LOADING) {
					e++;
					const s = Z(t, U.CHANGE, (r) => {
						const o = t.getState();
						if (o == M.LOADED || o == M.ERROR || o == M.EMPTY) {
							se(s);
							e--;
							this.unlistenSources_();
							this.reproject_();
						}
					});
					this.sourcesListenerKeys_.push(s);
				}
			});
			e === 0 ? setTimeout(this.reproject_.bind(this), 0) : this.sourceTiles_.forEach(function({ tile: t }, i, s) {
				if (t.getState() == M.IDLE) {
					t.load();
				}
			});
		}
	}
	unlistenSources_() {
		this.sourcesListenerKeys_.forEach(se);
		this.sourcesListenerKeys_ = null;
	}
	release() {
		if (this.canvas_) {
			Ns(this.canvas_.getContext("2d"));
			Cn.push(this.canvas_);
			this.canvas_ = null;
		}
		this.sourceTiles_.length = 0;
		super.release();
	}
}
class TE {
	constructor(e) {
		this.highWaterMark = e !== "undefined" ? e : 2048;
		this.count_ = 0;
		this.entries_ = {};
		this.oldest_ = null;
		this.newest_ = null;
	}
	deleteOldest() {
		const e = this.pop();
		if (e instanceof Tf) {
			e.dispose();
		}
	}
	canExpireCache() {
		return this.highWaterMark > 0 && this.getCount() > this.highWaterMark;
	}
	expireCache(e) {
		for (; this.canExpireCache();) this.deleteOldest();
	}
	clear() {
		for (; this.oldest_;) this.deleteOldest();
	}
	containsKey(e) {
		return this.entries_.hasOwnProperty(e);
	}
	forEach(e) {
		let t = this.oldest_;
		for (; t;) {
			e(t.value_, t.key_, this);
			t = t.newer;
		}
	}
	get(e, t) {
		const i = this.entries_[e];
		ee(this.entries_[e] !== "undefined", "Tried to get a value for a key that does not exist in the cache");
		if (!(this.entries_[e] === this.newest_)) {
			this.entries_[e] === this.oldest_ ? (this.oldest_ = this.oldest_.newer, this.oldest_.older = null) : (this.entries_[e].newer.older = this.entries_[e].older, this.entries_[e].older.newer = this.entries_[e].newer);
			this.entries_[e].newer = null;
			this.entries_[e].older = this.newest_;
			this.newest_.newer = this.entries_[e];
			this.newest_ = this.entries_[e];
		}
		return this.entries_[e].value_;
	}
	remove(e) {
		const t = this.entries_[e];
		ee(this.entries_[e] !== "undefined", "Tried to get a value for a key that does not exist in the cache");
		this.entries_[e] === this.newest_ ? (this.newest_ = this.entries_[e].older, this.newest_ && (this.newest_.newer = null)) : this.entries_[e] === this.oldest_ ? (this.oldest_ = this.entries_[e].newer, this.oldest_ && (this.oldest_.older = null)) : (this.entries_[e].newer.older = this.entries_[e].older, this.entries_[e].older.newer = this.entries_[e].newer);
		delete this.entries_[e];
		--this.count_;
		return this.entries_[e].value_;
	}
	getCount() {
		return this.count_;
	}
	getKeys() {
		const e = new Array(this.count_);
		let t = 0, i;
		for (i = this.newest_; i; i = i.older) e[t++] = i.key_;
		return e;
	}
	getValues() {
		const e = new Array(this.count_);
		let t = 0, i;
		for (i = this.newest_; i; i = i.older) e[t++] = i.value_;
		return e;
	}
	peekLast() {
		return this.oldest_.value_;
	}
	peekLastKey() {
		return this.oldest_.key_;
	}
	peekFirstKey() {
		return this.newest_.key_;
	}
	peek(e) {
		var t;
		return (t = this.entries_[e]) == null ? "undefined" : t.value_;
	}
	pop() {
		const e = this.oldest_;
		delete this.entries_[this.oldest_.key_];
		if (this.oldest_.newer) {
			this.oldest_.newer.older = null;
		}
		this.oldest_ = this.oldest_.newer;
		if (!this.oldest_) {
			this.newest_ = null;
		}
		--this.count_;
		return this.oldest_.value_;
	}
	replace(e, t) {
		this.get(e);
		this.entries_[e].value_ = t;
	}
	set(e, t) {
		ee(!(e in this.entries_), "Tried to set a value for a key that is used already");
		const i = {
			key_: e,
			newer: null,
			older: this.newest_,
			value_: t
		};
		this.newest_ ? this.newest_.newer = i : this.oldest_ = i;
		this.newest_ = i;
		this.entries_[e] = i;
		++this.count_;
	}
	setSize(e) {
		this.highWaterMark = e;
	}
}
function Kr(n, e, t, i) {
	return i !== "undefined" ? (i[0] = n, i[1] = e, i[2] = t, i) : [
		n,
		e,
		t
	];
}
function CE(n, e, t) {
	return n + "/" + e + "/" + t;
}
function Rr(n, e, t, i, s) {
	return "".concat(O(n), ",").concat(e, ",").concat(CE(t, i, s));
}
function Gd(n) {
	return RE(n[0], n[1], n[2]);
}
function RE(n, e, t) {
	return (e << n) + t;
}
function SE(n, e) {
	const t = n[0], i = n[1], s = n[2];
	if (e.getMinZoom() > n[0] || n[0] > e.getMaxZoom()) return false;
	const r = e.getFullTileRange(n[0]);
	return r ? r.containsXY(n[1], n[2]) : true;
}
function ta(n, e, t) {
	if (!(t in n)) return n[t] = new Set([e]), true;
	const i = n[t], s = n[t].has(e);
	if (!s) {
		n[t].add(e);
	}
	return !s;
}
function vE(n, e, t) {
	const i = n[t];
	return n[t] ? n[t].delete(e) : false;
}
function bc(n, e) {
	const t = n.layerStatesArray[n.layerIndex];
	if (n.layerStatesArray[n.layerIndex].extent) {
		e = Et(e, lt(n.layerStatesArray[n.layerIndex].extent, n.viewState.projection));
	}
	const i = n.layerStatesArray[n.layerIndex].layer.getRenderSource();
	if (!i.getWrapX()) {
		const s = i.getTileGridForProjection(n.viewState.projection).getExtent();
		if (s) {
			e = Et(e, s);
		}
	}
	return e;
}
class wE extends Im {
	constructor(e, t) {
		super(e);
		t = t || {};
		this.extentChanged = true;
		this.renderComplete = false;
		this.renderedExtent_ = null;
		this.renderedPixelRatio;
		this.renderedProjection = null;
		this.renderedTiles = [];
		this.renderedSourceKey_;
		this.renderedSourceRevision_;
		this.tempExtent = je();
		this.tempTileRange_ = new Ed(0, 0, 0, 0);
		this.tempTileCoord_ = Kr(0, 0, 0);
		const i = t.cacheSize !== "undefined" ? t.cacheSize : 512;
		this.tileCache_ = new TE(i);
		this.sourceTileCache_ = null;
		this.maxStaleKeys = i * .5;
	}
	getTileCache() {
		return this.tileCache_;
	}
	getSourceTileCache() {
		if (!this.sourceTileCache_) {
			this.sourceTileCache_ = new TE(512);
		}
		return this.sourceTileCache_;
	}
	getOrCreateTile(e, t, i, s) {
		const r = this.tileCache_, a = this.getLayer().getSource(), l = Rr(a, a.getKey(), e, t, i);
		let h;
		if (this.tileCache_.containsKey(l)) h = this.tileCache_.get(l);
		else {
			const c = s.viewState.projection;
			const u = a.getProjection();
			h = a.getTile(e, t, i, s.pixelRatio, s.viewState.projection, !u || Ae(u, s.viewState.projection) ? "undefined" : this.getSourceTileCache());
			if (!h) return null;
			r.set(l, h);
		}
		return h;
	}
	getTile(e, t, i, s) {
		const r = this.getOrCreateTile(e, t, i, s);
		return r || null;
	}
	getData(e) {
		const t = this.frameState;
		if (!this.frameState) return null;
		const i = this.getLayer(), s = xe(this.frameState.pixelToCoordinateTransform, e.slice()), r = i.getExtent();
		if (r && !Ui(r, s)) return null;
		const o = this.frameState.viewState, a = i.getRenderSource(), l = a.getTileGridForProjection(this.frameState.viewState.projection), h = a.getTilePixelRatio(this.frameState.pixelRatio);
		for (let c = l.getZForResolution(this.frameState.viewState.resolution); c >= l.getMinZoom(); --c) {
			const u = l.getTileCoordForCoordAndZ(s, c);
			const d = this.getTile(c, u[1], u[2], t);
			if (!d || d.getState() !== M.LOADED) continue;
			const f = l.getOrigin(c);
			const g = Ne(l.getTileSize(c));
			const m = l.getResolution(c);
			let _;
			if (d instanceof Ap || d instanceof EE) _ = d.getImage();
			else if (d instanceof _E) {
				_ = Ua(d.getData());
				if (!_) continue;
			} else continue;
			const p = Math.floor(h * ((s[0] - f[0]) / m - u[1] * g[0]));
			const y = Math.floor(h * ((f[1] - s[1]) / m - u[2] * g[1]));
			const E = Math.round(h * a.getGutterForProjection(o.projection));
			return this.getImageData(_, p + E, y + E);
		}
		return null;
	}
	prepareFrame(e) {
		var s;
		this.renderedProjection ? e.viewState.projection !== this.renderedProjection && (this.tileCache_.clear(), this.renderedProjection = e.viewState.projection) : this.renderedProjection = e.viewState.projection;
		const t = this.getLayer().getSource();
		if (!t) return false;
		const i = t.getRevision();
		this.renderedSourceRevision_ ? this.renderedSourceRevision_ !== i && (this.renderedSourceRevision_ = i, this.renderedSourceKey_ === t.getKey() && (this.tileCache_.clear(), (s = this.sourceTileCache_) == null || s.clear())) : this.renderedSourceRevision_ = i;
		return true;
	}
	enqueueTilesForNextExtent() {
		return true;
	}
	enqueueTiles(e, t, i, s, r) {
		const o = e.viewState, a = this.getLayer(), l = a.getRenderSource(), h = l.getTileGridForProjection(e.viewState.projection), c = O(l);
		if (!(c in e.wantedTiles)) {
			e.wantedTiles[c] = {};
		}
		const u = e.wantedTiles[c], d = a.getMapInternal(), f = Math.max(i - r, h.getMinZoom(), h.getZForResolution(Math.min(a.getMaxResolution(), d ? d.getView().getResolutionForZoom(Math.max(a.getMinZoom(), 0)) : h.getResolution(0)), l.zDirection)), g = e.viewState.rotation, m = e.viewState.rotation ? Jc(e.viewState.center, e.viewState.resolution, e.viewState.rotation, e.size) : "undefined";
		for (let _ = i; _ >= f; --_) {
			const p = h.getTileRangeForExtentAndZ(t, _, this.tempTileRange_);
			const y = h.getResolution(_);
			for (let E = p.minX; E <= p.maxX; ++E) for (let x = p.minY; x <= p.maxY; ++x) {
				if (g && !h.tileCoordIntersectsViewport([
					_,
					E,
					x
				], m)) continue;
				const T = this.getTile(_, E, x, e);
				if (!T || !ta(s, T, _)) continue;
				const P = T.getKey();
				u[P] = true;
				if (T.getState() === M.IDLE && !e.tileQueue.isKeyQueued(P)) {
					const S = Kr(_, E, x, this.tempTileCoord_);
					e.tileQueue.enqueue([
						T,
						c,
						h.getTileCoordCenter(S),
						y
					]);
				}
			}
		}
	}
	findStaleTile_(e, t) {
		const i = this.tileCache_, s = e[0], r = e[1], o = e[2], a = this.getStaleKeys();
		for (let l = 0; l < a.length; ++l) {
			const h = Rr(this.getLayer().getSource(), a[l], s, r, o);
			if (i.containsKey(h)) {
				const c = i.peek(h);
				if (c.getState() === M.LOADED) return c.endTransition(O(this)), ta(t, c, s), true;
			}
		}
		return false;
	}
	findAltTiles_(e, t, i, s) {
		const r = e.getTileRangeForTileCoordAndZ(t, i, this.tempTileRange_);
		if (!r) return false;
		let o = true;
		const a = this.tileCache_, l = this.getLayer().getRenderSource(), h = l.getKey();
		for (let c = r.minX; c <= r.maxX; ++c) for (let u = r.minY; u <= r.maxY; ++u) {
			const d = Rr(l, h, i, c, u);
			let f = false;
			if (a.containsKey(d)) {
				const g = a.peek(d);
				if (g.getState() === M.LOADED) {
					ta(s, g, i);
					f = true;
				}
			}
			o = false;
		}
		return o;
	}
	renderFrame(e, t) {
		this.renderComplete = true;
		const i = e.layerStatesArray[e.layerIndex], s = e.viewState, r = e.viewState.projection, o = e.viewState.resolution, a = e.viewState.center, l = e.pixelRatio, h = this.getLayer(), c = h.getSource(), u = c.getTileGridForProjection(e.viewState.projection), d = u.getZForResolution(e.viewState.resolution, c.zDirection), f = u.getResolution(d), g = c.getKey();
		this.renderedSourceKey_ ? this.renderedSourceKey_ !== g && (this.prependStaleKey(this.renderedSourceKey_), this.renderedSourceKey_ = g) : this.renderedSourceKey_ = g;
		let m = e.extent;
		const _ = c.getTilePixelRatio(e.pixelRatio);
		this.prepareContainer(e, t);
		const p = this.context.canvas.width, y = this.context.canvas.height, E = e.layerStatesArray[e.layerIndex].extent && lt(e.layerStatesArray[e.layerIndex].extent);
		if (E) {
			m = Et(m, lt(e.layerStatesArray[e.layerIndex].extent));
		}
		const x = f * this.context.canvas.width / 2 / _, T = f * this.context.canvas.height / 2 / _, v = [
			e.viewState.center[0] - x,
			e.viewState.center[1] - T,
			e.viewState.center[0] + x,
			e.viewState.center[1] + T
		], P = {};
		this.renderedTiles.length = 0;
		const S = h.getPreload();
		if (e.nextExtent && this.enqueueTilesForNextExtent()) {
			const k = u.getZForResolution(s.nextResolution, c.zDirection);
			const B = bc(e, e.nextExtent);
			this.enqueueTiles(e, B, k, P, S);
		}
		const R = bc(e, m);
		this.enqueueTiles(e, R, d, P, 0);
		if (S > 0) {
			setTimeout(() => {
				this.enqueueTiles(e, R, d - 1, P, S - 1);
			}, 0);
		}
		if (!(d in P)) return this.container;
		const I = O(this), N = e.time;
		for (const k of P[d]) {
			const B = k.getState();
			if (B === M.EMPTY) continue;
			const q = k.tileCoord;
			if (B === M.LOADED && k.getAlpha(I, N) === 1) {
				k.endTransition(I);
				continue;
			}
			if (B !== M.ERROR) {
				this.renderComplete = false;
			}
			if (this.findStaleTile_(k.tileCoord, P)) {
				vE(P, k, d);
				e.animate = true;
				continue;
			}
			if (this.findAltTiles_(u, k.tileCoord, d + 1, P)) continue;
			const ce = u.getMinZoom();
			for (let Fe = d - 1; Fe >= ce && !this.findAltTiles_(u, k.tileCoord, Fe, P); --Fe);
		}
		const L = f / e.viewState.resolution * e.pixelRatio / _, A = this.getRenderContext(e);
		gt(this.tempTransform, this.context.canvas.width / 2, this.context.canvas.height / 2, L, L, 0, -this.context.canvas.width / 2, -this.context.canvas.height / 2);
		if (e.layerStatesArray[e.layerIndex].extent) {
			this.clipUnrotated(A, e, E);
		}
		if (!c.getInterpolate()) {
			A.imageSmoothingEnabled = false;
		}
		this.preRender(A, e);
		const W = Object.keys(P).map(Number);
		W.sort(Lt);
		let w;
		const b = [], D = [];
		for (let k = W.length - 1; k >= 0; --k) {
			const B = W[k];
			const q = c.getTilePixelSize(W[k], l, r);
			const re = u.getResolution(W[k]) / f;
			const ce = q[0] * re * L;
			const Fe = q[1] * re * L;
			const ve = u.getTileCoordForCoordAndZ(ri(v), W[k]);
			const ze = u.getTileCoordExtent(ve);
			const F = xe(this.tempTransform, [_ * (ze[0] - v[0]) / f, _ * (v[3] - ze[3]) / f]);
			const ai = _ * c.getGutterForProjection(r);
			for (const it of P[W[k]]) {
				if (it.getState() !== M.LOADED) continue;
				const Xe = it.tileCoord;
				const Xs = ve[1] - it.tileCoord[1];
				const To = Math.round(F[0] - (Xs - 1) * ce);
				const Xn = ve[2] - it.tileCoord[2];
				const Ws = Math.round(F[1] - (Xn - 1) * Fe);
				const mt = Math.round(F[0] - Xs * ce);
				const St = Math.round(F[1] - Xn * Fe);
				const Ki = To - mt;
				const Li = Ws - St;
				const li = W.length === 1;
				let Ai = false;
				w = [
					mt,
					St,
					mt + Ki,
					St,
					mt + Ki,
					St + Li,
					mt,
					St + Li
				];
				for (let Hi = 0, Co = b.length; Hi < Co; ++Hi) if (!li && B < D[Hi]) {
					const We = b[Hi];
					if (me([
						mt,
						St,
						mt + Ki,
						St + Li
					], [
						b[Hi][0],
						b[Hi][3],
						b[Hi][4],
						b[Hi][7]
					])) {
						A.save();
						Ai = true;
						A.beginPath();
						A.moveTo(w[0], w[1]);
						A.lineTo(w[2], w[3]);
						A.lineTo(w[4], w[5]);
						A.lineTo(w[6], w[7]);
						A.moveTo(b[Hi][6], b[Hi][7]);
						A.lineTo(b[Hi][4], b[Hi][5]);
						A.lineTo(b[Hi][2], b[Hi][3]);
						A.lineTo(b[Hi][0], b[Hi][1]);
						A.clip();
					}
				}
				b.push(w);
				D.push(B);
				this.drawTile(it, e, mt, St, Ki, Li, ai, li);
				this.renderedTiles.unshift(it);
				this.updateUsedTiles(e.usedTiles, c, it);
			}
		}
		this.renderedResolution = f;
		this.extentChanged = !this.renderedExtent_ || !Si(this.renderedExtent_, v);
		this.renderedExtent_ = v;
		this.renderedPixelRatio = e.pixelRatio;
		this.postRender(this.context, e);
		if (e.layerStatesArray[e.layerIndex].extent) {
			A.restore();
		}
		A.imageSmoothingEnabled = true;
		if (this.renderComplete) {
			const k = (B, q) => {
				var Fe;
				const ie = O(c), re = q.wantedTiles[ie], ce = q.wantedTiles[ie] ? Object.keys(q.wantedTiles[ie]).length : 0;
				this.updateCacheSize(ce);
				this.tileCache_.expireCache();
				if (!((Fe = this.sourceTileCache_) == null)) {
					Fe.expireCache();
				}
			};
			e.postRenderFunctions.push(k);
		}
		return this.container;
	}
	updateCacheSize(e) {
		this.tileCache_.highWaterMark = Math.max(this.tileCache_.highWaterMark, e * 2);
	}
	drawTile(e, t, i, s, r, o, a, l) {
		let h;
		if (e instanceof _E) {
			h = Ua(e.getData());
			if (!h) throw new Error("Rendering array data is not yet supported");
		} else h = this.getTileImage(e);
		if (!h) return;
		const c = this.getRenderContext(t), u = O(this), d = t.layerStatesArray[t.layerIndex], f = t.layerStatesArray[t.layerIndex].opacity * (l ? e.getAlpha(u, t.time) : 1), g = f !== c.globalAlpha;
		if (g) {
			c.save();
			c.globalAlpha = f;
		}
		c.drawImage(h, a, a, h.width - 2 * a, h.height - 2 * a, i, s, r, o);
		if (g) {
			c.restore();
		}
		f !== t.layerStatesArray[t.layerIndex].opacity ? t.animate = true : l && e.endTransition(u);
	}
	getImage() {
		const e = this.context;
		return this.context ? this.context.canvas : null;
	}
	getTileImage(e) {
		return e.getImage();
	}
	updateUsedTiles(e, t, i) {
		const s = O(t);
		if (!(s in e)) {
			e[s] = {};
		}
		e[s][i.getKey()] = true;
	}
}
const Ti = {
	PRELOAD: "preload",
	USE_INTERIM_TILES_ON_ERROR: "useInterimTilesOnError"
};
class PE extends wp {
	constructor(e) {
		e = e || {};
		const t = Object.assign({}, e), i = e.cacheSize;
		delete e.cacheSize;
		delete t.preload;
		delete t.useInterimTilesOnError;
		super(t);
		this.on;
		this.once;
		this.un;
		this.cacheSize_ = e.cacheSize;
		this.setPreload(e.preload !== "undefined" ? e.preload : 0);
		this.setUseInterimTilesOnError(e.useInterimTilesOnError !== "undefined" ? e.useInterimTilesOnError : true);
	}
	getCacheSize() {
		return this.cacheSize_;
	}
	getPreload() {
		return this.get(Ti.PRELOAD);
	}
	setPreload(e) {
		this.set(Ti.PRELOAD, e);
	}
	getUseInterimTilesOnError() {
		return this.get(Ti.USE_INTERIM_TILES_ON_ERROR);
	}
	setUseInterimTilesOnError(e) {
		this.set(Ti.USE_INTERIM_TILES_ON_ERROR, e);
	}
	getData(e) {
		return super.getData(e);
	}
}
class FE extends PE {
	constructor(e) {
		super(e);
	}
	createRenderer() {
		return new wE(this, { cacheSize: this.getCacheSize() });
	}
}
const rn = [
	0,
	0,
	0
];
const fi = 5;
class LE {
	constructor(e) {
		this.minZoom = e.minZoom !== "undefined" ? e.minZoom : 0;
		this.resolutions_ = e.resolutions;
		ee(vf(this.resolutions_, (s, r) => r - s, true), "`resolutions` must be sorted in descending order");
		let t;
		if (!e.origins) {
			for (let s = 0, r = this.resolutions_.length - 1; s < r; ++s) if (!t) t = this.resolutions_[s] / this.resolutions_[s + 1];
			else if (this.resolutions_[s] / this.resolutions_[s + 1] !== t) {
				t = "undefined";
				break;
			}
		}
		this.zoomFactor_ = t;
		this.maxZoom = this.resolutions_.length - 1;
		this.origin_ = e.origin !== "undefined" ? e.origin : null;
		this.origins_ = null;
		if (e.origins !== "undefined") {
			this.origins_ = e.origins;
			ee(this.origins_.length == this.resolutions_.length, "Number of `origins` and `resolutions` must be equal");
		}
		const i = e.extent;
		if (e.extent !== "undefined" && !this.origin_ && !this.origins_) {
			this.origin_ = ri(e.extent);
		}
		ee(!this.origin_ && this.origins_ || this.origin_ && !this.origins_, "Either `origin` or `origins` must be configured, never both");
		this.tileSizes_ = null;
		if (e.tileSizes !== "undefined") {
			this.tileSizes_ = e.tileSizes;
			ee(this.tileSizes_.length == this.resolutions_.length, "Number of `tileSizes` and `resolutions` must be equal");
		}
		this.tileSize_ = e.tileSize !== "undefined" ? e.tileSize : this.tileSizes_ ? null : Wl;
		ee(!this.tileSize_ && this.tileSizes_ || this.tileSize_ && !this.tileSizes_, "Either `tileSize` or `tileSizes` must be configured, never both");
		this.extent_ = e.extent !== "undefined" ? e.extent : null;
		this.fullTileRanges_ = null;
		this.tmpSize_ = [0, 0];
		this.tmpExtent_ = [
			0,
			0,
			0,
			0
		];
		e.sizes !== "undefined" ? this.fullTileRanges_ = e.sizes.map((s, r) => {
			const o = new Ed(Math.min(0, s[0]), Math.max(s[0] - 1, -1), Math.min(0, s[1]), Math.max(s[1] - 1, -1));
			if (i) {
				const a = this.getTileRangeForExtentAndZ(i, r);
				o.minX = Math.max(a.minX, o.minX);
				o.maxX = Math.min(a.maxX, o.maxX);
				o.minY = Math.max(a.minY, o.minY);
				o.maxY = Math.min(a.maxY, o.maxY);
			}
			return o;
		}) : e.extent && this.calculateTileRanges_(e.extent);
	}
	forEachTileCoord(e, t, i) {
		const s = this.getTileRangeForExtentAndZ(e, t);
		for (let r = s.minX, o = s.maxX; r <= o; ++r) for (let a = s.minY, l = s.maxY; a <= l; ++a) i([
			t,
			r,
			a
		]);
	}
	forEachTileCoordParentTileRange(e, t, i, s) {
		let r, o, a, l = null, h = e[0] - 1;
		for (this.zoomFactor_ === 2 ? (o = e[1], a = e[2]) : l = this.getTileCoordExtent(e, s); h >= this.minZoom;) {
			o !== "undefined" && a !== "undefined" ? (o = Math.floor(o / 2), a = Math.floor(a / 2), r = nn(o, o, a, a, i)) : r = this.getTileRangeForExtentAndZ(l, h, i);
			if (t(h, r)) return true;
			--h;
		}
		return false;
	}
	getExtent() {
		return this.extent_;
	}
	getMaxZoom() {
		return this.maxZoom;
	}
	getMinZoom() {
		return this.minZoom;
	}
	getOrigin(e) {
		return this.origin_ ? this.origin_ : this.origins_[e];
	}
	getOrigins() {
		return this.origins_;
	}
	getResolution(e) {
		return this.resolutions_[e];
	}
	getResolutions() {
		return this.resolutions_;
	}
	getTileCoordChildTileRange(e, t, i) {
		if (e[0] < this.maxZoom) {
			if (this.zoomFactor_ === 2) {
				const r = e[1] * 2;
				const o = e[2] * 2;
				return nn(r, r + 1, o, o + 1, t);
			}
			const s = this.getTileCoordExtent(e, i || this.tmpExtent_);
			return this.getTileRangeForExtentAndZ(s, e[0] + 1, t);
		}
		return null;
	}
	getTileRangeForTileCoordAndZ(e, t, i) {
		if (t > this.maxZoom || t < this.minZoom) return null;
		const s = e[0], r = e[1], o = e[2];
		if (t === e[0]) return nn(e[1], e[2], e[1], e[2], i);
		if (this.zoomFactor_) {
			const l = Math.pow(this.zoomFactor_, t - s);
			const h = Math.floor(r * l);
			const c = Math.floor(o * l);
			if (t < s) return nn(h, h, c, c, i);
			const u = Math.floor(l * (r + 1)) - 1;
			const d = Math.floor(l * (o + 1)) - 1;
			return nn(h, u, c, d, i);
		}
		const a = this.getTileCoordExtent(e, this.tmpExtent_);
		return this.getTileRangeForExtentAndZ(a, t, i);
	}
	getTileRangeForExtentAndZ(e, t, i) {
		this.getTileCoordForXYAndZ_(e[0], e[3], t, false, rn);
		const s = rn[1], r = rn[2];
		this.getTileCoordForXYAndZ_(e[2], e[1], t, true, rn);
		const o = rn[1], a = rn[2];
		return nn(rn[1], rn[1], rn[2], rn[2], i);
	}
	getTileCoordCenter(e) {
		const t = this.getOrigin(e[0]), i = this.getResolution(e[0]), s = Ne(this.getTileSize(e[0]), this.tmpSize_);
		return [t[0] + (e[1] + .5) * s[0] * i, t[1] - (e[2] + .5) * s[1] * i];
	}
	getTileCoordExtent(e, t) {
		const i = this.getOrigin(e[0]), s = this.getResolution(e[0]), r = Ne(this.getTileSize(e[0]), this.tmpSize_), o = i[0] + e[1] * r[0] * s, a = i[1] - (e[2] + 1) * r[1] * s, l = o + r[0] * s, h = a + r[1] * s;
		return bt(o, a, l, h, t);
	}
	getTileCoordForCoordAndResolution(e, t, i) {
		return this.getTileCoordForXYAndResolution_(e[0], e[1], t, false, i);
	}
	getTileCoordForXYAndResolution_(e, t, i, s, r) {
		const o = this.getZForResolution(i), a = i / this.getResolution(o), l = this.getOrigin(o), h = Ne(this.getTileSize(o), this.tmpSize_);
		let c = a * (e - l[0]) / i / h[0], u = a * (l[1] - t) / i / h[1];
		s ? (c = _i(c, fi) - 1, u = _i(u, fi) - 1) : (c = gn(c, fi), u = gn(u, fi));
		return Kr(o, c, u, r);
	}
	getTileCoordForXYAndZ_(e, t, i, s, r) {
		const o = this.getOrigin(i), a = this.getResolution(i), l = Ne(this.getTileSize(i), this.tmpSize_);
		let h = (e - o[0]) / a / l[0], c = (o[1] - t) / a / l[1];
		s ? (h = _i(h, fi) - 1, c = _i(c, fi) - 1) : (h = gn(h, fi), c = gn(c, fi));
		return Kr(i, h, c, r);
	}
	getTileCoordForCoordAndZ(e, t, i) {
		return this.getTileCoordForXYAndZ_(e[0], e[1], t, false, i);
	}
	getTileCoordResolution(e) {
		return this.resolutions_[e[0]];
	}
	getTileSize(e) {
		return this.tileSize_ ? this.tileSize_ : this.tileSizes_[e];
	}
	getFullTileRange(e) {
		return this.fullTileRanges_ ? this.fullTileRanges_[e] : this.extent_ ? this.getTileRangeForExtentAndZ(this.extent_, e) : null;
	}
	getZForResolution(e, t) {
		const i = io(this.resolutions_, e, t || 0);
		return fe(i, this.minZoom, this.maxZoom);
	}
	tileCoordIntersectsViewport(e, t) {
		return vu(t, 0, t.length, 2, this.getTileCoordExtent(e));
	}
	calculateTileRanges_(e) {
		const t = this.resolutions_.length, i = new Array(this.resolutions_.length);
		for (let s = this.minZoom; s < this.resolutions_.length; ++s) i[s] = this.getTileRangeForExtentAndZ(e, s);
		this.fullTileRanges_ = i;
	}
}
class $d extends LE {
	constructor(e) {
		super({
			extent: e.extent,
			origin: e.origin,
			origins: e.origins,
			resolutions: e.resolutions,
			tileSize: e.tileSize,
			tileSizes: e.tileSizes,
			sizes: e.sizes
		});
		this.matrixIds_ = e.matrixIds;
	}
	getMatrixId(e) {
		return this.matrixIds_[e];
	}
	getMatrixIds() {
		return this.matrixIds_;
	}
}
function AE(n, e, t) {
	const i = [], s = [], r = [], o = [], a = [];
	t = t !== "undefined" ? t : [];
	const l = "SupportedCRS", h = "TileMatrix", c = "Identifier", u = "ScaleDenominator", d = "TopLeftCorner", f = "TileWidth", g = "TileHeight", m = n[l], _ = H(n[l]), p = _.getMetersPerUnit(), y = _.getAxisOrientation().startsWith("ne");
	n[h].sort(function(E, x) {
		return x[u] - E[u];
	});
	n[h].forEach(function(E) {
		let x;
		t.length > 0 ? x = t.find(function(T) {
			return E[c] == T[h] ? true : E[c].includes(":") ? false : n[c] + ":" + E[c] === T[h];
		}) : x = true;
		if (x) {
			s.push(E[c]);
			const T = E[u] * 28e-5 / p;
			const v = E[f];
			const P = E[g];
			y ? r.push([E[d][1], E[d][0]]) : r.push(E[d]);
			i.push(T);
			o.push(E[f] == E[g] ? E[f] : [E[f], E[g]]);
			a.push([E.MatrixWidth, E.MatrixHeight]);
		}
	});
	return new $d({
		extent: e,
		origins: r,
		resolutions: i,
		matrixIds: s,
		tileSizes: o,
		sizes: a
	});
}
function rh(n) {
	let e = n.getDefaultTileGrid();
	if (!e) {
		e = OE(n);
		n.setDefaultTileGrid(e);
	}
	return e;
}
function ME(n, e, t) {
	const i = e[0], s = n.getTileCoordCenter(e), r = xo(t);
	if (!Ui(r, s)) {
		const o = J(r);
		const a = Math.ceil((r[0] - s[0]) / o);
		s[0] += o * a;
		return n.getTileCoordForCoordAndZ(s, i);
	}
	return e;
}
function bE(n, e, t, i) {
	i = i !== "undefined" ? i : "top-left";
	const s = jd(n, e, t);
	return new LE({
		extent: n,
		origin: Of(n, i),
		resolutions: s,
		tileSize: t
	});
}
function Ud(n) {
	const e = n || {}, t = e.extent || H("EPSG:3857").getExtent(), i = {
		extent: t,
		minZoom: e.minZoom,
		tileSize: e.tileSize,
		resolutions: jd(t, e.maxZoom, e.tileSize, e.maxResolution)
	};
	return new LE(i);
}
function jd(n, e, t, i) {
	e = e !== "undefined" ? e : cd;
	t = Ne(t !== "undefined" ? t : Wl);
	const s = Ce(n), r = J(n);
	i = i > 0 ? i : Math.max(r / t[0], s / t[1]);
	const o = e + 1, a = new Array(o);
	for (let l = 0; l < o; ++l) a[l] = i / Math.pow(2, l);
	return a;
}
function OE(n, e, t, i) {
	const s = xo(n);
	return bE(s, e, t, i);
}
function xo(n) {
	n = H(n);
	let e = n.getExtent();
	if (!e) {
		const t = 180 * _s.degrees / n.getMetersPerUnit();
		e = bt(-t, -t, t, t);
	}
	return e;
}
function An(n, e) {
	const t = [];
	Object.keys(e).forEach(function(s) {
		if (e[s] !== null && e[s] !== "undefined") {
			t.push(s + "=" + encodeURIComponent(e[s]));
		}
	});
	const i = t.join("&");
	n = n.replace(/[?&]$/, "");
	n += n.includes("?") ? "&" : "?";
	return n + i;
}
const DE = /\{z\}/g;
const NE = /\{x\}/g;
const kE = /\{y\}/g;
const GE = /\{-y\}/g;
function BE(n, e, t, i, s) {
	return n.replace(DE, e.toString()).replace(NE, t.toString()).replace(kE, i.toString()).replace(GE, function() {
		return (s - i).toString();
	});
}
function zd(n) {
	const e = [];
	let t = /\{([a-z])-([a-z])\}/.exec(n);
	if (t) {
		const i = t[1].charCodeAt(0);
		const s = t[2].charCodeAt(0);
		let r;
		for (r = i; r <= s; ++r) e.push(n.replace(t[0], String.fromCharCode(r)));
		return e;
	}
	t = /\{(\d+)-(\d+)\}/.exec(n);
	if (t) {
		const i = parseInt(t[2], 10);
		for (let s = parseInt(t[1], 10); s <= i; s++) e.push(n.replace(t[0], s.toString()));
		return e;
	}
	e.push(n);
	return e;
}
function $E(n, e) {
	return function(t, i, s) {
		if (!t) return;
		let r;
		const o = t[0];
		if (e) {
			const a = e.getFullTileRange(o);
			if (a) {
				r = a.getHeight() - 1;
			}
		}
		return BE(n, t[0], t[1], t[2], r);
	};
}
function UE(n, e) {
	const t = n.length, i = new Array(n.length);
	for (let s = 0; s < n.length; ++s) i[s] = $E(n[s], e);
	return ja(i);
}
function ja(n) {
	return n.length === 1 ? n[0] : function(e, t, i) {
		if (!e) return;
		const s = Gd(e), r = Qt(s, n.length);
		return n[r](e, t, i);
	};
}
class jE extends a_ {
	constructor(e) {
		super({
			attributions: e.attributions,
			attributionsCollapsible: e.attributionsCollapsible,
			projection: e.projection,
			state: e.state,
			wrapX: e.wrapX,
			interpolate: e.interpolate
		});
		this.on;
		this.once;
		this.un;
		this.tilePixelRatio_ = e.tilePixelRatio !== "undefined" ? e.tilePixelRatio : 1;
		this.tileGrid = e.tileGrid !== "undefined" ? e.tileGrid : null;
		const t = [256, 256];
		if (this.tileGrid) {
			Ne(this.tileGrid.getTileSize(this.tileGrid.getMinZoom()), t);
		}
		this.tmpSize = [0, 0];
		this.key_ = e.key || O(this);
		this.tileOptions = {
			transition: e.transition,
			interpolate: e.interpolate
		};
		this.zDirection = e.zDirection ? e.zDirection : 0;
	}
	getGutterForProjection(e) {
		return 0;
	}
	getKey() {
		return this.key_;
	}
	setKey(e) {
		if (this.key_ !== e) {
			this.key_ = e;
			this.changed();
		}
	}
	getResolutions(e) {
		const t = e ? this.getTileGridForProjection(e) : this.tileGrid;
		return t ? t.getResolutions() : null;
	}
	getTile(e, t, i, s, r, o) {
		return z();
	}
	getTileGrid() {
		return this.tileGrid;
	}
	getTileGridForProjection(e) {
		return this.tileGrid ? this.tileGrid : rh(e);
	}
	getTilePixelRatio(e) {
		return this.tilePixelRatio_;
	}
	getTilePixelSize(e, t, i) {
		const s = this.getTileGridForProjection(i), r = this.getTilePixelRatio(t), o = Ne(s.getTileSize(e), this.tmpSize);
		return r == 1 ? o : Q_(o, r, this.tmpSize);
	}
	getTileCoordForTileUrlFunction(e, t) {
		const i = t !== "undefined" ? t : this.getProjection(), s = t !== "undefined" ? this.getTileGridForProjection(i) : this.tileGrid || this.getTileGridForProjection(i);
		if (this.getWrapX() && i.isGlobal()) {
			e = ME(s, e, i);
		}
		return SE(e, s) ? e : null;
	}
	clear() {}
	refresh() {
		this.clear();
		super.refresh();
	}
}
class zE extends wf {
	constructor(e, t) {
		super(e);
		this.tile = t;
	}
}
const ia = {
	TILELOADSTART: "tileloadstart",
	TILELOADEND: "tileloadend",
	TILELOADERROR: "tileloaderror"
};
class oh extends jE {
	constructor(e) {
		super({
			attributions: e.attributions,
			cacheSize: e.cacheSize,
			projection: e.projection,
			state: e.state,
			tileGrid: e.tileGrid,
			tilePixelRatio: e.tilePixelRatio,
			wrapX: e.wrapX,
			transition: e.transition,
			interpolate: e.interpolate,
			key: e.key,
			attributionsCollapsible: e.attributionsCollapsible,
			zDirection: e.zDirection
		});
		this.generateTileUrlFunction_ = this.tileUrlFunction === oh.prototype.tileUrlFunction;
		this.tileLoadFunction = e.tileLoadFunction;
		if (e.tileUrlFunction) {
			this.tileUrlFunction = e.tileUrlFunction;
		}
		this.urls = null;
		e.urls ? this.setUrls(e.urls) : e.url && this.setUrl(e.url);
		this.tileLoadingKeys_ = {};
	}
	getTileLoadFunction() {
		return this.tileLoadFunction;
	}
	getTileUrlFunction() {
		return Object.getPrototypeOf(this).tileUrlFunction === this.tileUrlFunction ? this.tileUrlFunction.bind(this) : this.tileUrlFunction;
	}
	getUrls() {
		return this.urls;
	}
	handleTileChange(e) {
		const t = e.target, i = O(e.target), s = e.target.getState();
		let r;
		s == M.LOADING ? (this.tileLoadingKeys_[i] = true, r = ia.TILELOADSTART) : i in this.tileLoadingKeys_ && (delete this.tileLoadingKeys_[i], r = s == M.ERROR ? ia.TILELOADERROR : s == M.LOADED ? ia.TILELOADEND : "undefined");
		if (r != null) {
			this.dispatchEvent(new zE(r, e.target));
		}
	}
	setTileLoadFunction(e) {
		this.tileLoadFunction = e;
		this.changed();
	}
	setTileUrlFunction(e, t) {
		this.tileUrlFunction = e;
		typeof t < "u" ? this.setKey(t) : this.changed();
	}
	setUrl(e) {
		const t = zd(e);
		this.urls = t;
		this.setUrls(t);
	}
	setUrls(e) {
		this.urls = e;
		const t = e.join("\n");
		this.generateTileUrlFunction_ ? this.setTileUrlFunction(UE(e, this.tileGrid), t) : this.setKey(t);
	}
	tileUrlFunction(e, t, i) {}
}
class WE extends oh {
	constructor(e) {
		super({
			attributions: e.attributions,
			cacheSize: e.cacheSize,
			projection: e.projection,
			state: e.state,
			tileGrid: e.tileGrid,
			tileLoadFunction: e.tileLoadFunction ? e.tileLoadFunction : VE,
			tilePixelRatio: e.tilePixelRatio,
			tileUrlFunction: e.tileUrlFunction,
			url: e.url,
			urls: e.urls,
			wrapX: e.wrapX,
			transition: e.transition,
			interpolate: e.interpolate !== "undefined" ? e.interpolate : true,
			key: e.key,
			attributionsCollapsible: e.attributionsCollapsible,
			zDirection: e.zDirection
		});
		this.crossOrigin = e.crossOrigin !== "undefined" ? e.crossOrigin : null;
		this.referrerPolicy = e.referrerPolicy;
		this.tileClass = e.tileClass !== "undefined" ? e.tileClass : Ap;
		this.tileGridForProjection = {};
		this.reprojectionErrorThreshold_ = e.reprojectionErrorThreshold;
		this.renderReprojectionEdges_ = false;
	}
	getGutterForProjection(e) {
		return this.getProjection() && e && !Ae(this.getProjection(), e) ? 0 : this.getGutter();
	}
	getGutter() {
		return 0;
	}
	getKey() {
		let e = super.getKey();
		if (!this.getInterpolate()) {
			e += ":disable-interpolation";
		}
		return e;
	}
	getTileGridForProjection(e) {
		const t = this.getProjection();
		if (this.tileGrid && (!t || Ae(t, e))) return this.tileGrid;
		const i = O(e);
		if (!(i in this.tileGridForProjection)) {
			this.tileGridForProjection[i] = rh(e);
		}
		return this.tileGridForProjection[i];
	}
	createTile_(e, t, i, s, r, o) {
		const a = [
			e,
			t,
			i
		], l = this.getTileCoordForTileUrlFunction(a, r), h = l ? this.tileUrlFunction(l, s, r) : "undefined", c = new this.tileClass(a, h !== "undefined" ? M.IDLE : M.EMPTY, h !== "undefined" ? h : "", {
			crossOrigin: this.crossOrigin,
			referrerPolicy: this.referrerPolicy
		}, this.tileLoadFunction, this.tileOptions);
		c.key = o;
		c.addEventListener(U.CHANGE, this.handleTileChange.bind(this));
		return c;
	}
	getTile(e, t, i, s, r, o) {
		const a = this.getProjection();
		if (!a || !r || Ae(a, r)) return this.getTileInternal(e, t, i, s, a || r);
		const l = [
			e,
			t,
			i
		], h = this.getKey(), c = this.getTileGridForProjection(a), u = this.getTileGridForProjection(r), d = this.getTileCoordForTileUrlFunction(l, r), f = new EE(a, c, r, u, l, d, this.getTilePixelRatio(s), this.getGutter(), (g, m, _, p) => this.getTileInternal(g, m, _, p, a, o), this.reprojectionErrorThreshold_, this.renderReprojectionEdges_, this.tileOptions);
		f.key = h;
		return f;
	}
	getTileInternal(e, t, i, s, r, o) {
		const a = this.getKey(), l = Rr(this, a, e, t, i);
		if (o && o.containsKey(l)) return o.get(l);
		const h = this.createTile_(e, t, i, s, r, a);
		if (!(o == null)) {
			o.set(l, h);
		}
		return h;
	}
	setRenderReprojectionEdges(e) {
		if (this.renderReprojectionEdges_ != e) {
			this.renderReprojectionEdges_ = e;
			this.changed();
		}
	}
	setTileGridForProjection(e, t) {
		const i = H(e);
		if (i) {
			const s = O(i);
			if (!(s in this.tileGridForProjection)) {
				this.tileGridForProjection[s] = t;
			}
		}
	}
}
function VE(n, e) {
	if (ht) {
		const t = n.getCrossOrigin();
		let i = "same-origin";
		let s = "same-origin";
		t === "anonymous" || t === "" ? (i = "cors", s = "omit") : t === "use-credentials" && (i = "cors", s = "include");
		const r = {
			mode: i,
			credentials: s,
			referrerPolicy: n.getReferrerPolicy()
		};
		fetch(e, r).then((o) => {
			if (!o.ok) throw new Error("HTTP ".concat(o.status));
			return o.blob();
		}).then((o) => createImageBitmap(o)).then((o) => {
			var h;
			const a = n.getImage();
			a.width = o.width;
			a.height = o.height;
			a.getContext("2d").drawImage(o, 0, 0);
			if (!((h = o.close) == null)) {
				h.call(o);
			}
			a.dispatchEvent(new Event("load"));
		}).catch(() => {
			n.getImage().dispatchEvent(new Event("error"));
		});
		return;
	}
	n.getImage().src = e;
}
class YE extends WE {
	constructor(e) {
		e = e || {};
		const t = e.projection !== "undefined" ? e.projection : "EPSG:3857", i = e.tileGrid !== "undefined" ? e.tileGrid : Ud({
			extent: xo(t),
			maxResolution: e.maxResolution,
			maxZoom: e.maxZoom,
			minZoom: e.minZoom,
			tileSize: e.tileSize
		});
		super({
			attributions: e.attributions,
			cacheSize: e.cacheSize,
			crossOrigin: e.crossOrigin,
			referrerPolicy: e.referrerPolicy,
			interpolate: e.interpolate,
			projection: t,
			reprojectionErrorThreshold: e.reprojectionErrorThreshold,
			tileGrid: i,
			tileLoadFunction: e.tileLoadFunction,
			tilePixelRatio: e.tilePixelRatio,
			tileUrlFunction: e.tileUrlFunction,
			url: e.url,
			urls: e.urls,
			wrapX: e.wrapX !== "undefined" ? e.wrapX : true,
			transition: e.transition,
			attributionsCollapsible: e.attributionsCollapsible,
			zDirection: e.zDirection
		});
		this.gutter_ = e.gutter !== "undefined" ? e.gutter : 0;
	}
	getGutter() {
		return this.gutter_;
	}
}
function Ci(n, e) {
	const t = n.length;
	return e < 0 ? n[e + n.length] : e >= n.length ? n[e - n.length] : n[e];
}
function yn(n, e) {
	const t = n.length;
	let i = Math.floor(e);
	const s = e - i;
	i >= n.length ? i -= n.length : i < 0 && (i += n.length);
	let r = i + 1;
	if (r >= n.length) {
		r -= n.length;
	}
	const o = n[i], a = n[i][0], l = n[i][1], h = n[r], c = n[r][0] - n[i][0], u = n[r][1] - n[i][1];
	return [n[i][0] + c * s, n[i][1] + u * s];
}
const hr = {
	index: -1,
	endIndex: NaN,
	closestTargetDistance: null
};
function Wd(n, e, t, i) {
	const s = n[0], r = n[1];
	let o = null, a = -1, l = NaN;
	for (let u = 0; u < e.targets.length; ++u) {
		const d = e.targets[u];
		const f = e.targets[u].coordinates;
		let g = null;
		let m;
		for (let _ = 0; _ < e.targets[u].coordinates.length - 1; ++_) {
			const p = f[_];
			const y = f[_ + 1];
			const E = Zd(s, r, f[_], f[_ + 1]);
			if (E.squaredDistance < g) {
				g = E.squaredDistance;
				m = _ + E.along;
			}
		}
		if (g < o) {
			o = g;
			if (e.targets[u].ring && e.targetIndex === u) {
				e.targets[u].endIndex > e.targets[u].startIndex ? m < e.targets[u].startIndex && (m += d.coordinates.length) : e.targets[u].endIndex < e.targets[u].startIndex && m > e.targets[u].startIndex && (m -= d.coordinates.length);
			}
			l = m;
			a = u;
		}
	}
	const h = e.targets[a];
	let c = e.targets[a].ring;
	if (e.targetIndex === a && c) {
		const u = yn(h.coordinates, l);
		const d = t.getPixelFromCoordinate(u);
		const f = t.getPixelFromCoordinate(e.startCoord);
		if (_n(d, f) > i) {
			c = false;
		}
	}
	if (c) {
		const u = h.coordinates;
		const d = h.coordinates.length;
		const f = h.startIndex;
		const g = l;
		if (h.startIndex < l) {
			const m = dr(u, f, g);
			if (dr(u, f, g - d) < m) {
				l -= d;
			}
		} else {
			const m = dr(u, f, g);
			if (dr(u, f, g + d) < m) {
				l += d;
			}
		}
	}
	hr.index = a;
	hr.endIndex = l;
	hr.closestTargetDistance = o;
	return hr;
}
function Vd(n, e) {
	const t = [];
	for (let i = 0; i < e.length; ++i) {
		const r = e[i].getGeometry();
		Yd(n, r, t);
	}
	return t;
}
function Yd(n, e, t) {
	if (e instanceof Nr) {
		cr(n, e.getCoordinates(), false, t);
		return;
	}
	if (e instanceof kr) {
		const i = e.getCoordinates();
		for (let s = 0, r = i.length; s < r; ++s) cr(n, i[s], false, t);
		return;
	}
	if (e instanceof wn) {
		const i = e.getCoordinates();
		for (let s = 0, r = i.length; s < r; ++s) cr(n, i[s], true, t);
		return;
	}
	if (e instanceof Gr) {
		const i = e.getCoordinates();
		for (let s = 0, r = i.length; s < r; ++s) {
			const o = i[s];
			for (let a = 0, l = i[s].length; a < l; ++a) cr(n, i[s][a], true, t);
		}
		return;
	}
	if (e instanceof br) {
		const i = e.getGeometries();
		for (let s = 0; s < i.length; ++s) Yd(n, i[s], t);
		return;
	}
}
function cr(n, e, t, i) {
	const s = n[0], r = n[1];
	for (let o = 0, a = e.length - 1; o < a; ++o) {
		const l = e[o];
		const h = e[o + 1];
		const c = Zd(s, r, e[o], e[o + 1]);
		if (c.squaredDistance === 0) {
			const u = o + c.along;
			i.push({
				coordinates: e,
				ring: t,
				startIndex: u,
				endIndex: u
			});
			return;
		}
	}
}
function ur(n, e) {
	return Jt(n[0], n[1], e[0], e[1]);
}
function dr(n, e, t) {
	let i, s;
	e < t ? (i = e, s = t) : (i = t, s = e);
	const r = Math.ceil(i), o = Math.floor(s);
	if (r > o) {
		const l = yn(n, i);
		const h = yn(n, s);
		return ur(l, h);
	}
	let a = 0;
	if (i < r) {
		const l = yn(n, i);
		const h = Ci(n, r);
		a += ur(l, h);
	}
	if (o < s) {
		const l = Ci(n, o);
		const h = yn(n, s);
		a += ur(l, h);
	}
	for (let l = r; l < o - 1; ++l) {
		const h = Ci(n, l);
		const c = Ci(n, l + 1);
		a += ur(h, c);
	}
	return a;
}
const na = {
	along: 0,
	squaredDistance: 0
};
function Zd(n, e, t, i) {
	const s = t[0], r = t[1], o = i[0], a = i[1], l = i[0] - t[0], h = i[1] - t[1];
	let c = 0, u = t[0], d = t[1];
	if (l !== 0 || h !== 0) {
		c = fe(((n - t[0]) * l + (e - t[1]) * h) / (l * l + h * h), 0, 1);
		u += l * c;
		d += h * c;
	}
	na.along = c;
	na.squaredDistance = bn(Jt(n, e, u, d), 10);
	return na;
}
const fr = {
	DRAWSTART: "drawstart",
	DRAWEND: "drawend",
	DRAWABORT: "drawabort"
};
class gr extends wf {
	constructor(e, t) {
		super(e);
		this.feature = t;
	}
}
class ZE extends oy {
	constructor(e) {
		const t = e;
		if (!e.stopDown) {
			e.stopDown = Xi;
		}
		super(e);
		this.on;
		this.once;
		this.un;
		this.options_ = e;
		this.shouldHandle_ = false;
		this.downPx_ = null;
		this.downTimeout_;
		this.lastDragTime_;
		this.pointerType_;
		this.freehand_ = false;
		this.source_ = e.source ? e.source : null;
		this.features_ = e.features ? e.features : null;
		this.snapTolerance_ = e.snapTolerance ? e.snapTolerance : 12;
		this.type_ = e.type;
		this.mode_ = HE(this.type_);
		this.stopClick_ = !!e.stopClick;
		this.ignoreNextUpEvent_ = false;
		this.minPoints_ = e.minPoints ? e.minPoints : this.mode_ === "Polygon" ? 3 : 2;
		this.maxPoints_ = this.mode_ === "Circle" ? 2 : e.maxPoints ? e.maxPoints : null;
		this.finishCondition_ = e.finishCondition ? e.finishCondition : ni;
		this.geometryLayout_ = e.geometryLayout ? e.geometryLayout : "XY";
		let i = e.geometryFunction;
		if (!i) {
			const s = this.mode_;
			if (this.mode_ === "Circle") i = (r, o, a) => {
				const l = o || new Eu([NaN, NaN]), h = ue(r[0]), c = ei(h, ue(r[r.length - 1]));
				l.setCenterAndRadius(h, Math.sqrt(c), this.geometryLayout_);
				return l;
			};
			else {
				let r;
				s === "Point" ? r = pl : s === "LineString" ? r = Nr : s === "Polygon" && (r = wn);
				i = (o, a, l) => a ? s === "Polygon" ? o[0].length ? a.setCoordinates([o[0].concat([o[0][0]])], this.geometryLayout_) : a.setCoordinates([], this.geometryLayout_) : a.setCoordinates(o, this.geometryLayout_) : a = new r(o, this.geometryLayout_);
			}
		}
		this.geometryFunction_ = i;
		this.dragVertexDelay_ = e.dragVertexDelay !== "undefined" ? e.dragVertexDelay : 500;
		this.finishCoordinate_ = null;
		this.sketchFeature_ = null;
		this.sketchPoint_ = null;
		this.sketchCoords_ = null;
		this.sketchLine_ = null;
		this.sketchLineCoords_ = null;
		this.squaredClickTolerance_ = e.clickTolerance ? e.clickTolerance * e.clickTolerance : 36;
		this.overlay_ = new Fp({
			source: new l_({
				useSpatialIndex: false,
				wrapX: e.wrapX ? e.wrapX : false
			}),
			style: e.style ? e.style : KE(),
			updateWhileInteracting: true
		});
		this.geometryName_ = e.geometryName;
		this.condition_ = e.condition ? e.condition : Hl;
		this.freehandCondition_;
		e.freehand ? this.freehandCondition_ = ni : this.freehandCondition_ = e.freehandCondition ? e.freehandCondition : Zr;
		this.traceCondition_;
		this.setTrace(e.trace || false);
		this.traceState_ = { active: false };
		this.traceSource_ = e.traceSource || e.source || null;
		this.addChangeListener(Da.ACTIVE, this.updateState_);
	}
	setTrace(e) {
		let t;
		e ? e === true ? t = ni : t = e : t = Xi;
		this.traceCondition_ = t;
	}
	setMap(e) {
		super.setMap(e);
		this.updateState_();
	}
	setFreehand(e) {
		this.freehand_ = e;
		this.freehand_ ? this.freehandCondition_ = ni : this.freehandCondition_ = this.options_ && this.options_.freehandCondition ? this.options_.freehandCondition : Zr;
	}
	getOverlay() {
		return this.overlay_;
	}
	getFreehand() {
		return this.freehand_;
	}
	handleEvent(e) {
		if (e.originalEvent.type === U.CONTEXTMENU) {
			e.originalEvent.preventDefault();
		}
		this.freehand_ = this.mode_ !== "Point" && this.freehandCondition_(e);
		let t = e.type === Q.POINTERMOVE, i = true;
		if (!this.freehand_ && this.lastDragTime_ && e.type === Q.POINTERDRAG) {
			Date.now() - this.lastDragTime_ >= this.dragVertexDelay_ ? (this.downPx_ = e.pixel, this.shouldHandle_ = !this.freehand_, t = true) : this.lastDragTime_ = "undefined";
			if (this.shouldHandle_ && this.downTimeout_ !== "undefined") {
				clearTimeout(this.downTimeout_);
				this.downTimeout_ = "undefined";
			}
		}
		this.freehand_ && e.type === Q.POINTERDRAG && this.sketchFeature_ !== null ? (this.addToDrawing_(e.coordinate), i = false) : this.freehand_ && e.type === Q.POINTERDOWN ? i = false : t && this.getPointerCount() < 2 ? (i = e.type === Q.POINTERMOVE, i && this.freehand_ ? (this.handlePointerMove_(e), this.shouldHandle_ && e.originalEvent.preventDefault()) : (e.originalEvent.pointerType === "mouse" || e.type === Q.POINTERDRAG && this.downTimeout_ === "undefined") && this.handlePointerMove_(e)) : e.type === Q.DBLCLICK && (i = false);
		return super.handleEvent(e) && i;
	}
	handleDownEvent(e) {
		this.shouldHandle_ = !this.freehand_;
		return this.freehand_ ? (this.downPx_ = e.pixel, this.finishCoordinate_ || this.startDrawing_(e.coordinate), true) : this.condition_(e) ? (this.lastDragTime_ = Date.now(), this.downTimeout_ = setTimeout(() => {
			this.handlePointerMove_(new Np(Q.POINTERMOVE, e.map, e.originalEvent, false, e.frameState));
		}, this.dragVertexDelay_), this.downPx_ = e.pixel, true) : (this.lastDragTime_ = "undefined", false);
	}
	deactivateTrace_() {
		this.traceState_ = { active: false };
	}
	toggleTraceState_(e) {
		if (!this.traceSource_ || !this.traceCondition_(e)) return;
		if (this.traceState_.active) {
			this.deactivateTrace_();
			return;
		}
		const t = this.getMap(), i = t.getCoordinateFromPixel([e.pixel[0] - this.snapTolerance_, e.pixel[1] + this.snapTolerance_]), s = t.getCoordinateFromPixel([e.pixel[0] + this.snapTolerance_, e.pixel[1] - this.snapTolerance_]), r = ye([i, s]), o = this.traceSource_.getFeaturesInExtent(r);
		if (o.length === 0) return;
		const a = Vd(e.coordinate, o);
		if (a.length) {
			this.traceState_ = {
				active: true,
				startCoord: e.coordinate.slice(),
				targets: a,
				targetIndex: -1
			};
		}
	}
	addOrRemoveTracedCoordinates_(e, t) {
		const i = e.startIndex <= e.endIndex, s = e.startIndex <= t;
		i === s ? i && t > e.endIndex || !i && t < e.endIndex ? this.addTracedCoordinates_(e, e.endIndex, t) : (i && t < e.endIndex || !i && t > e.endIndex) && this.removeTracedCoordinates_(t, e.endIndex) : (this.removeTracedCoordinates_(e.startIndex, e.endIndex), this.addTracedCoordinates_(e, e.startIndex, t));
	}
	removeTracedCoordinates_(e, t) {
		let i = 0;
		if (e < t) {
			const s = Math.ceil(e);
			let r = Math.floor(t);
			if (r === t) {
				r -= 1;
			}
			i = r - s + 1;
		} else {
			const s = Math.floor(e);
			let r = Math.ceil(t);
			if (r === t) {
				r += 1;
			}
			i = s - r + 1;
		}
		if (i > 0) {
			this.removeLastPoints_(i);
		}
	}
	addTracedCoordinates_(e, t, i) {
		const s = [];
		if (t < i) {
			const r = Math.ceil(t);
			let o = Math.floor(i);
			if (o === i) {
				o -= 1;
			}
			for (let a = r; a <= o; ++a) s.push(Ci(e.coordinates, a));
		} else {
			const r = Math.floor(t);
			let o = Math.ceil(i);
			if (o === i) {
				o += 1;
			}
			for (let a = r; a >= o; --a) s.push(Ci(e.coordinates, a));
		}
		if (s.length) {
			this.appendCoordinates(s);
		}
	}
	updateTrace_(e) {
		const t = this.traceState_;
		if (!this.traceState_.active) return;
		if (this.traceState_.targetIndex === -1) {
			const a = e.map.getPixelFromCoordinate(t.startCoord);
			if (_n(a, e.pixel) < this.snapTolerance_) return;
		}
		const i = Wd(e.coordinate, this.traceState_, this.getMap(), this.snapTolerance_);
		if (this.traceState_.targetIndex !== i.index) {
			if (t.targetIndex !== -1) {
				const l = t.targets[t.targetIndex];
				this.removeTracedCoordinates_(t.targets[t.targetIndex].startIndex, t.targets[t.targetIndex].endIndex);
			}
			const a = t.targets[i.index];
			this.addTracedCoordinates_(t.targets[i.index], t.targets[i.index].startIndex, i.endIndex);
		} else {
			const a = t.targets[t.targetIndex];
			this.addOrRemoveTracedCoordinates_(t.targets[t.targetIndex], i.endIndex);
		}
		this.traceState_.targetIndex = i.index;
		const s = this.traceState_.targets[this.traceState_.targetIndex];
		this.traceState_.targets[this.traceState_.targetIndex].endIndex = i.endIndex;
		const r = yn(this.traceState_.targets[this.traceState_.targetIndex].coordinates, this.traceState_.targets[this.traceState_.targetIndex].endIndex), o = this.getMap().getPixelFromCoordinate(r);
		e.coordinate = r;
		e.pixel = [Math.round(o[0]), Math.round(o[1])];
	}
	handleDragEvent(e) {
		this.ignoreNextUpEvent_ = true;
		super.handleDragEvent(e);
	}
	handleUpEvent(e) {
		let t = true;
		if (this.getPointerCount() === 0) {
			if (this.downTimeout_) {
				clearTimeout(this.downTimeout_);
				this.downTimeout_ = "undefined";
			}
			this.handlePointerMove_(e);
			const i = this.traceState_.active;
			if (!this.ignoreNextUpEvent_) {
				this.toggleTraceState_(e);
			}
			if (this.shouldHandle_) {
				const s = !this.finishCoordinate_;
				if (s) {
					this.startDrawing_(e.coordinate);
				}
				!s && this.freehand_ ? this.finishDrawing() : !this.freehand_ && (!s || this.mode_ === "Point") && (this.atFinish_(e.pixel, i) ? this.finishCondition_(e) && this.finishDrawing() : this.addToDrawing_(e.coordinate));
				t = false;
			} else this.freehand_ && this.abortDrawing();
		}
		this.ignoreNextUpEvent_ = false;
		if (!t && this.stopClick_) {
			e.preventDefault();
		}
		return t;
	}
	handlePointerMove_(e) {
		this.pointerType_ = e.originalEvent.pointerType;
		if (this.downPx_ && (!this.freehand_ && this.shouldHandle_ || this.freehand_ && !this.shouldHandle_)) {
			const t = this.downPx_;
			const i = e.pixel;
			const s = this.downPx_[0] - e.pixel[0];
			const r = this.downPx_[1] - e.pixel[1];
			const o = s * s + r * r;
			this.shouldHandle_ = this.freehand_ ? o > this.squaredClickTolerance_ : o <= this.squaredClickTolerance_;
			if (!this.shouldHandle_) return;
		}
		if (!this.finishCoordinate_) {
			this.createOrUpdateSketchPoint_(e.coordinate.slice());
			return;
		}
		this.updateTrace_(e);
		this.modifyDrawing_(e.coordinate);
	}
	atFinish_(e, t) {
		let i = false;
		if (this.sketchFeature_) {
			let s = false;
			let r = [this.finishCoordinate_];
			const o = this.mode_;
			if (this.mode_ === "Point") i = true;
			else if (this.mode_ === "Circle") i = this.sketchCoords_.length === 2;
			else if (this.mode_ === "LineString") s = !t && this.sketchCoords_.length > this.minPoints_;
			else if (this.mode_ === "Polygon") {
				const a = this.sketchCoords_;
				s = this.sketchCoords_[0].length > this.minPoints_;
				r = [this.sketchCoords_[0][0], this.sketchCoords_[0][this.sketchCoords_[0].length - 2]];
				t ? r = [this.sketchCoords_[0][0]] : r = [this.sketchCoords_[0][0], this.sketchCoords_[0][this.sketchCoords_[0].length - 2]];
			}
		}
		return i;
	}
	createOrUpdateSketchPoint_(e) {
		this.sketchPoint_ ? this.sketchPoint_.getGeometry().setCoordinates(e) : (this.sketchPoint_ = new sl(new pl(e)), this.updateSketchFeatures_());
	}
	createOrUpdateCustomSketchLine_(e) {
		if (!this.sketchLine_) {
			this.sketchLine_ = new sl();
		}
		const t = e.getLinearRing(0);
		let i = this.sketchLine_.getGeometry();
		i ? (i.setFlatCoordinates(t.getLayout(), t.getFlatCoordinates()), i.changed()) : (i = new Nr(t.getFlatCoordinates(), t.getLayout()), this.sketchLine_.setGeometry(i));
	}
	startDrawing_(e) {
		const t = this.getMap().getView().getProjection(), i = Mr(this.geometryLayout_);
		for (; e.length < i;) e.push(0);
		this.finishCoordinate_ = e;
		this.mode_ === "Point" ? this.sketchCoords_ = e.slice() : this.mode_ === "Polygon" ? (this.sketchCoords_ = [[e.slice(), e.slice()]], this.sketchLineCoords_ = this.sketchCoords_[0]) : this.sketchCoords_ = [e.slice(), e.slice()];
		if (this.sketchLineCoords_) {
			this.sketchLine_ = new sl(new Nr(this.sketchLineCoords_));
		}
		const s = this.geometryFunction_(this.sketchCoords_, "undefined", t);
		this.sketchFeature_ = new sl();
		if (this.geometryName_) {
			this.sketchFeature_.setGeometryName(this.geometryName_);
		}
		this.sketchFeature_.setGeometry(s);
		this.updateSketchFeatures_();
		this.dispatchEvent(new gr(fr.DRAWSTART, this.sketchFeature_));
	}
	modifyDrawing_(e) {
		const t = this.getMap(), i = this.sketchFeature_.getGeometry(), s = t.getView().getProjection(), r = Mr(this.geometryLayout_);
		let o, a;
		for (; e.length < r;) e.push(0);
		this.mode_ === "Point" ? a = this.sketchCoords_ : this.mode_ === "Polygon" ? (o = this.sketchCoords_[0], a = o[o.length - 1], this.atFinish_(t.getPixelFromCoordinate(e)) && (e = this.finishCoordinate_.slice())) : (o = this.sketchCoords_, a = o[o.length - 1]);
		a[0] = e[0];
		a[1] = e[1];
		this.geometryFunction_(this.sketchCoords_, i, s);
		if (this.sketchPoint_) {
			this.sketchPoint_.getGeometry().setCoordinates(e);
		}
		i.getType() === "Polygon" && this.mode_ !== "Polygon" ? this.createOrUpdateCustomSketchLine_(i) : this.sketchLineCoords_ && this.sketchLine_.getGeometry().setCoordinates(this.sketchLineCoords_);
		this.updateSketchFeatures_();
	}
	addToDrawing_(e) {
		const t = this.sketchFeature_.getGeometry(), i = this.getMap().getView().getProjection();
		let s, r;
		const o = this.mode_;
		this.mode_ === "LineString" || this.mode_ === "Circle" ? (this.finishCoordinate_ = e.slice(), r = this.sketchCoords_, r.length >= this.maxPoints_ && (this.freehand_ ? r.pop() : s = true), r.push(e.slice()), this.geometryFunction_(r, t, i)) : this.mode_ === "Polygon" && (r = this.sketchCoords_[0], r.length >= this.maxPoints_ && (this.freehand_ ? r.pop() : s = true), r.push(e.slice()), s && (this.finishCoordinate_ = r[0]), this.geometryFunction_(this.sketchCoords_, t, i));
		this.createOrUpdateSketchPoint_(e.slice());
		this.updateSketchFeatures_();
		return s ? this.finishDrawing() : this.sketchFeature_;
	}
	removeLastPoints_(e) {
		if (!this.sketchFeature_) return;
		const t = this.sketchFeature_.getGeometry(), i = this.getMap().getView().getProjection(), s = this.mode_;
		for (let r = 0; r < e; ++r) {
			let o;
			if (s === "LineString" || s === "Circle") {
				o = this.sketchCoords_;
				o.splice(-2, 1);
				if (o.length >= 2) {
					this.finishCoordinate_ = o[o.length - 2].slice();
					const a = this.finishCoordinate_.slice();
					o[o.length - 1] = a;
					this.createOrUpdateSketchPoint_(a);
				}
				this.geometryFunction_(o, t, i);
				if (t.getType() === "Polygon" && this.sketchLine_) {
					this.createOrUpdateCustomSketchLine_(t);
				}
			} else if (s === "Polygon") {
				o = this.sketchCoords_[0];
				o.splice(-2, 1);
				const a = this.sketchLine_.getGeometry();
				if (o.length >= 2) {
					const l = o[o.length - 2].slice();
					o[o.length - 1] = l;
					this.createOrUpdateSketchPoint_(l);
				}
				a.setCoordinates(o);
				this.geometryFunction_(this.sketchCoords_, t, i);
			}
			if (o.length === 1) {
				this.abortDrawing();
				break;
			}
		}
		this.updateSketchFeatures_();
	}
	removeLastPoint() {
		this.removeLastPoints_(1);
	}
	finishDrawing() {
		const e = this.abortDrawing_();
		if (!e) return null;
		let t = this.sketchCoords_;
		const i = e.getGeometry(), s = this.getMap().getView().getProjection();
		this.mode_ === "LineString" ? (t.pop(), this.geometryFunction_(t, i, s)) : this.mode_ === "Polygon" && (t[0].pop(), this.geometryFunction_(t, i, s), t = i.getCoordinates());
		this.type_ === "MultiPoint" ? e.setGeometry(new yl([t])) : this.type_ === "MultiLineString" ? e.setGeometry(new kr([t])) : this.type_ === "MultiPolygon" && e.setGeometry(new Gr([t]));
		this.dispatchEvent(new gr(fr.DRAWEND, e));
		if (this.features_) {
			this.features_.push(e);
		}
		if (this.source_) {
			this.source_.addFeature(e);
		}
		return e;
	}
	abortDrawing_() {
		this.finishCoordinate_ = null;
		const e = this.sketchFeature_;
		this.sketchFeature_ = null;
		this.sketchPoint_ = null;
		this.sketchLine_ = null;
		this.overlay_.getSource().clear(true);
		this.deactivateTrace_();
		return this.sketchFeature_;
	}
	abortDrawing() {
		const e = this.abortDrawing_();
		if (e) {
			this.dispatchEvent(new gr(fr.DRAWABORT, e));
		}
	}
	appendCoordinates(e) {
		const t = this.mode_, i = !this.sketchFeature_;
		if (i) {
			this.startDrawing_(e[0]);
		}
		let s;
		if (this.mode_ === "LineString" || this.mode_ === "Circle") s = this.sketchCoords_;
		else if (this.mode_ === "Polygon") s = this.sketchCoords_ && this.sketchCoords_.length ? this.sketchCoords_[0] : [];
		else return;
		if (i) {
			s.shift();
		}
		s.pop();
		for (let o = 0; o < e.length; o++) this.addToDrawing_(e[o]);
		const r = e[e.length - 1];
		this.sketchFeature_ = this.addToDrawing_(e[e.length - 1]);
		this.modifyDrawing_(e[e.length - 1]);
	}
	extend(e) {
		const i = e.getGeometry();
		this.sketchFeature_ = e;
		this.sketchCoords_ = i.getCoordinates();
		const s = this.sketchCoords_[this.sketchCoords_.length - 1];
		this.finishCoordinate_ = this.sketchCoords_[this.sketchCoords_.length - 1].slice();
		this.sketchCoords_.push(this.sketchCoords_[this.sketchCoords_.length - 1].slice());
		this.sketchPoint_ = new sl(new pl(this.sketchCoords_[this.sketchCoords_.length - 1]));
		this.updateSketchFeatures_();
		this.dispatchEvent(new gr(fr.DRAWSTART, this.sketchFeature_));
	}
	updateSketchFeatures_() {
		const e = [];
		if (this.sketchFeature_) {
			e.push(this.sketchFeature_);
		}
		if (this.sketchLine_) {
			e.push(this.sketchLine_);
		}
		if (this.sketchPoint_) {
			e.push(this.sketchPoint_);
		}
		const t = this.overlay_.getSource();
		t.clear(true);
		t.addFeatures(e);
	}
	updateState_() {
		const e = this.getMap(), t = this.getActive();
		if (!e || !t) {
			this.abortDrawing();
		}
		this.overlay_.setMap(t ? e : null);
	}
}
function KE() {
	const n = bl();
	return function(e, t) {
		return n[e.getGeometry().getType()];
	};
}
function eR() {
	return function(n, e, t) {
		const i = ye([n[0], n[n.length - 1]].map(function(r) {
			return ue(r);
		})), s = [[
			Sn(i),
			As(i),
			Ms(i),
			ri(i),
			Sn(i)
		]];
		e ? e.setCoordinates(s) : e = new wn(s);
		return e;
	};
}
function HE(n) {
	switch (n) {
		case "Point":
		case "MultiPoint": return "Point";
		case "LineString":
		case "MultiLineString": return "LineString";
		case "Polygon":
		case "MultiPolygon": return "Polygon";
		case "Circle": return "Circle";
		default: throw new Error("Invalid type: " + n);
	}
}
const Oc = 0;
const ds = 1;
const Dc = [
	0,
	0,
	0,
	0
];
const Rn = [];
const sa = {
	MODIFYSTART: "modifystart",
	MODIFYEND: "modifyend"
};
function ra(n, e, t) {
	let i;
	switch (e) {
		case "LineString":
			i = n;
			break;
		case "MultiLineString":
		case "Polygon":
			i = n[t[0]];
			break;
		case "MultiPolygon":
			i = n[t[1]][t[0]];
			break;
	}
	return i;
}
class oa extends wf {
	constructor(e, t, i) {
		super(e);
		this.features = t;
		this.mapBrowserEvent = i;
	}
}
class qE extends oy {
	constructor(e) {
		super(e);
		this.handleSourceAdd_ = this.handleSourceAdd_.bind(this);
		this.handleSourceRemove_ = this.handleSourceRemove_.bind(this);
		this.handleExternalCollectionAdd_ = this.handleExternalCollectionAdd_.bind(this);
		this.handleExternalCollectionRemove_ = this.handleExternalCollectionRemove_.bind(this);
		this.handleFeatureChange_ = this.handleFeatureChange_.bind(this);
		this.on;
		this.once;
		this.un;
		this.condition_ = e.condition ? e.condition : pd;
		this.defaultDeleteCondition_ = function(i) {
			return iy(i) && _d(i);
		};
		this.deleteCondition_ = e.deleteCondition ? e.deleteCondition : this.defaultDeleteCondition_;
		this.insertVertexCondition_ = e.insertVertexCondition ? e.insertVertexCondition : ni;
		this.vertexFeature_ = null;
		this.vertexSegments_ = null;
		this.lastCoordinate_ = [0, 0];
		this.ignoreNextSingleClick_ = false;
		this.featuresBeingModified_ = null;
		this.rBush_ = new o_();
		this.pixelTolerance_ = e.pixelTolerance !== "undefined" ? e.pixelTolerance : 10;
		this.snappedToVertex_ = false;
		this.changingFeature_ = false;
		this.dragSegments_ = [];
		this.overlay_ = new Fp({
			source: new l_({
				useSpatialIndex: false,
				wrapX: !!e.wrapX
			}),
			style: e.style ? e.style : QE(),
			updateWhileAnimating: true,
			updateWhileInteracting: true
		});
		this.SEGMENT_WRITERS_ = {
			Point: this.writePointGeometry_.bind(this),
			LineString: this.writeLineStringGeometry_.bind(this),
			LinearRing: this.writeLineStringGeometry_.bind(this),
			Polygon: this.writePolygonGeometry_.bind(this),
			MultiPoint: this.writeMultiPointGeometry_.bind(this),
			MultiLineString: this.writeMultiLineStringGeometry_.bind(this),
			MultiPolygon: this.writeMultiPolygonGeometry_.bind(this),
			Circle: this.writeCircleGeometry_.bind(this),
			GeometryCollection: this.writeGeometryCollectionGeometry_.bind(this)
		};
		this.source_ = null;
		this.traceSource_ = e.traceSource || e.source || null;
		this.traceCondition_;
		this.setTrace(e.trace || false);
		this.traceState_ = { active: false };
		this.traceSegments_ = null;
		this.hitDetection_ = null;
		this.filterFunctionWasSupplied_ = e.filter != null;
		this.filter_ = e.filter ? e.filter : () => true;
		if (!(e.features || e.source)) throw new Error("The modify interaction requires features collection or a source");
		let t;
		e.features ? (t = e.features.getArray(), e.features.addEventListener(_e.ADD, this.handleExternalCollectionAdd_), e.features.addEventListener(_e.REMOVE, this.handleExternalCollectionRemove_), this.featuresCollection_ = e.features) : e.source && (t = e.source.getFeatures(), e.source.addEventListener(Te.ADDFEATURE, this.handleSourceAdd_), e.source.addEventListener(Te.REMOVEFEATURE, this.handleSourceRemove_), this.source_ = e.source);
		t.forEach((i) => {
			i.addEventListener(U.CHANGE, this.handleFeatureChange_);
			if (this.filterFunctionWasSupplied_) {
				i.addEventListener(xt.PROPERTYCHANGE, this.handleFeatureChange_);
			}
		});
		if (e.hitDetection) {
			this.hitDetection_ = e.hitDetection;
		}
		this.features_ = [];
		t.filter(this.filter_).forEach((i) => this.addFeature_(i));
		this.lastPointerEvent_ = null;
		this.delta_ = [0, 0];
		this.snapToPointer_ = e.snapToPointer === "undefined" ? !this.hitDetection_ : e.snapToPointer;
	}
	setTrace(e) {
		let t;
		e ? e === true ? t = ni : t = e : t = Xi;
		this.traceCondition_ = t;
	}
	addFeature_(e) {
		this.features_.push(e);
		const t = e.getGeometry();
		if (t) {
			const s = this.SEGMENT_WRITERS_[t.getType()];
			if (this.SEGMENT_WRITERS_[t.getType()]) {
				this.SEGMENT_WRITERS_[t.getType()](e, t);
			}
		}
		const i = this.getMap();
		if (i && i.isRendered() && this.getActive()) {
			this.handlePointerAtPixel_(this.lastCoordinate_);
		}
	}
	willModifyFeatures_(e, t) {
		if (!this.featuresBeingModified_) {
			this.featuresBeingModified_ = new Af();
			const i = this.featuresBeingModified_.getArray();
			for (let s = 0, r = t.length; s < r; ++s) {
				const o = t[s].feature;
				if (t[s].feature && !i.includes(t[s].feature)) {
					this.featuresBeingModified_.push(t[s].feature);
				}
			}
			this.featuresBeingModified_.getLength() === 0 ? this.featuresBeingModified_ = null : this.dispatchEvent(new oa(sa.MODIFYSTART, this.featuresBeingModified_, e));
		}
	}
	removeFeature_(e) {
		const t = this.features_.indexOf(e);
		this.features_.splice(t, 1);
		this.removeFeatureSegmentData_(e);
		if (this.vertexFeature_ && this.features_.length === 0) {
			this.overlay_.getSource().removeFeature(this.vertexFeature_);
			this.vertexFeature_ = null;
		}
	}
	removeFeatureSegmentData_(e) {
		const t = this.rBush_, i = [];
		this.rBush_.forEach(function(s) {
			if (e === s.feature) {
				i.push(s);
			}
		});
		for (let s = i.length - 1; s >= 0; --s) {
			const r = i[s];
			for (let o = this.dragSegments_.length - 1; o >= 0; --o) this.dragSegments_[o][0] === i[s] && this.dragSegments_.splice(o, 1);
			t.remove(i[s]);
		}
	}
	setActive(e) {
		if (this.vertexFeature_ && !e) {
			this.overlay_.getSource().removeFeature(this.vertexFeature_);
			this.vertexFeature_ = null;
		}
		super.setActive(e);
	}
	setMap(e) {
		this.overlay_.setMap(e);
		super.setMap(e);
	}
	getOverlay() {
		return this.overlay_;
	}
	handleSourceAdd_(e) {
		const t = e.feature;
		if (e.feature) {
			this.externalAddFeatureHandler_(e.feature);
		}
	}
	handleSourceRemove_(e) {
		const t = e.feature;
		if (e.feature) {
			this.externalRemoveFeatureHandler_(e.feature);
		}
	}
	handleExternalCollectionAdd_(e) {
		const t = e.element;
		if (e.element) {
			this.externalAddFeatureHandler_(e.element);
		}
	}
	handleExternalCollectionRemove_(e) {
		const t = e.element;
		if (e.element) {
			this.externalRemoveFeatureHandler_(e.element);
		}
	}
	externalAddFeatureHandler_(e) {
		e.addEventListener(U.CHANGE, this.handleFeatureChange_);
		if (this.filterFunctionWasSupplied_) {
			e.addEventListener(xt.PROPERTYCHANGE, this.handleFeatureChange_);
		}
		if (this.filter_(e)) {
			this.addFeature_(e);
		}
	}
	externalRemoveFeatureHandler_(e) {
		e.removeEventListener(U.CHANGE, this.handleFeatureChange_);
		if (this.filterFunctionWasSupplied_) {
			e.removeEventListener(xt.PROPERTYCHANGE, this.handleFeatureChange_);
		}
		this.removeFeature_(e);
	}
	handleFeatureChange_(e) {
		if (!this.changingFeature_) {
			const t = e.target;
			this.removeFeature_(e.target);
			if (this.filter_(e.target)) {
				this.addFeature_(e.target);
			}
		}
	}
	writePointGeometry_(e, t) {
		const i = t.getCoordinates(), s = {
			feature: e,
			geometry: t,
			segment: [i, i]
		};
		this.rBush_.insert(t.getExtent(), s);
	}
	writeMultiPointGeometry_(e, t) {
		const i = t.getCoordinates();
		for (let s = 0, r = i.length; s < r; ++s) {
			const o = i[s];
			const a = {
				feature: e,
				geometry: t,
				depth: [s],
				index: s,
				segment: [i[s], i[s]]
			};
			this.rBush_.insert(t.getExtent(), a);
		}
	}
	writeLineStringGeometry_(e, t) {
		const i = t.getCoordinates();
		for (let s = 0, r = i.length - 1; s < r; ++s) {
			const o = i.slice(s, s + 2);
			const a = {
				feature: e,
				geometry: t,
				index: s,
				segment: o
			};
			this.rBush_.insert(ye(o), a);
		}
	}
	writeMultiLineStringGeometry_(e, t) {
		const i = t.getCoordinates();
		for (let s = 0, r = i.length; s < r; ++s) {
			const o = i[s];
			for (let a = 0, l = i[s].length - 1; a < l; ++a) {
				const h = o.slice(a, a + 2);
				const c = {
					feature: e,
					geometry: t,
					depth: [s],
					index: a,
					segment: h
				};
				this.rBush_.insert(ye(h), c);
			}
		}
	}
	writePolygonGeometry_(e, t) {
		const i = t.getCoordinates();
		for (let s = 0, r = i.length; s < r; ++s) {
			const o = i[s];
			for (let a = 0, l = i[s].length - 1; a < l; ++a) {
				const h = o.slice(a, a + 2);
				const c = {
					feature: e,
					geometry: t,
					depth: [s],
					index: a,
					segment: h
				};
				this.rBush_.insert(ye(h), c);
			}
		}
	}
	writeMultiPolygonGeometry_(e, t) {
		const i = t.getCoordinates();
		for (let s = 0, r = i.length; s < r; ++s) {
			const o = i[s];
			for (let a = 0, l = i[s].length; a < l; ++a) {
				const h = o[a];
				for (let c = 0, u = o[a].length - 1; c < u; ++c) {
					const d = h.slice(c, c + 2);
					const f = {
						feature: e,
						geometry: t,
						depth: [a, s],
						index: c,
						segment: d
					};
					this.rBush_.insert(ye(d), f);
				}
			}
		}
	}
	writeCircleGeometry_(e, t) {
		const i = t.getCenter(), s = {
			feature: e,
			geometry: t,
			index: Oc,
			segment: [i, i]
		}, r = {
			feature: e,
			geometry: t,
			index: ds,
			segment: [i, i]
		}, o = [s, r];
		s.featureSegments = o;
		r.featureSegments = o;
		this.rBush_.insert(rs(i), s);
		let a = t;
		this.rBush_.insert(a.getExtent(), r);
	}
	writeGeometryCollectionGeometry_(e, t) {
		const i = t.getGeometriesArray();
		for (let s = 0; s < i.length; ++s) {
			const r = i[s];
			const o = this.SEGMENT_WRITERS_[i[s].getType()];
			this.SEGMENT_WRITERS_[i[s].getType()](e, i[s]);
		}
	}
	createOrUpdateVertexFeature_(e, t, i, s) {
		let r = this.vertexFeature_;
		r ? r.getGeometry().setCoordinates(e) : (r = new sl(new pl(e)), this.vertexFeature_ = r, this.overlay_.getSource().addFeature(r));
		r.set("features", t);
		r.set("geometries", i);
		r.set("existing", s);
		return r;
	}
	handleEvent(e) {
		if (!e.originalEvent) return true;
		this.lastPointerEvent_ = e;
		let t;
		if (!e.map.getView().getInteracting() && e.type == Q.POINTERMOVE && !this.handlingDownUpSequence) {
			this.handlePointerMove_(e);
		}
		if (this.vertexFeature_ && this.deleteCondition_(e)) {
			e.type != Q.SINGLECLICK || !this.ignoreNextSingleClick_ ? t = this.removePoint() : t = true;
		}
		if (e.type == Q.SINGLECLICK) {
			this.ignoreNextSingleClick_ = false;
		}
		return super.handleEvent(e) && !t;
	}
	findInsertVerticesAndUpdateDragSegments_(e) {
		this.handlePointerAtPixel_(e);
		this.dragSegments_.length = 0;
		this.featuresBeingModified_ = null;
		if (!this.vertexFeature_) return;
		this.getMap().getView().getProjection();
		const i = [], s = this.vertexFeature_.getGeometry().getCoordinates(), r = ye([s]), o = this.rBush_.getInExtent(r), a = {};
		o.sort(JE);
		for (let l = 0, h = o.length; l < h; ++l) {
			const c = o[l];
			const u = o[l].segment;
			let d = O(o[l].geometry);
			const f = o[l].depth;
			if (o[l].depth) {
				d += "-" + c.depth.join("-");
			}
			if (!a[d]) {
				a[d] = new Array(2);
			}
			if (o[l].geometry.getType() === "Circle" && o[l].index === ds) {
				const g = kc(e, c);
				if ($e(g, s) && !a[d][0]) {
					this.dragSegments_.push([c, 0]);
					a[d][0] = c;
				}
				continue;
			}
			if ($e(o[l].segment[0], s) && !a[d][0]) {
				this.dragSegments_.push([c, 0]);
				a[d][0] = c;
				continue;
			}
			if ($e(o[l].segment[1], s) && !a[d][1]) {
				if (a[d][0] && a[d][0].index === 0) {
					let g = c.geometry.getCoordinates();
					switch (c.geometry.getType()) {
						case "LineString":
						case "MultiLineString": continue;
						case "MultiPolygon": g = g[f[1]];
						case "Polygon":
							if (c.index !== g[f[0]].length - 2) continue;
							break;
					}
				}
				this.dragSegments_.push([c, 1]);
				a[d][1] = c;
				continue;
			}
			if (O(o[l].segment) in this.vertexSegments_ && !a[d][0] && !a[d][1]) {
				i.push(o[l]);
			}
		}
		return i;
	}
	deactivateTrace_() {
		this.traceState_ = { active: false };
	}
	updateTrace_(e) {
		const t = this.traceState_;
		if (!this.traceState_.active) return;
		if (this.traceState_.targetIndex === -1) {
			const r = e.map.getPixelFromCoordinate(t.startCoord);
			if (_n(r, e.pixel) < this.pixelTolerance_) return;
		}
		const i = Wd(e.coordinate, this.traceState_, e.map, this.pixelTolerance_);
		if (this.traceState_.targetIndex === -1 && Math.sqrt(i.closestTargetDistance) / e.map.getView().getResolution() > this.pixelTolerance_) return;
		if (this.traceState_.targetIndex !== i.index) {
			if (t.targetIndex !== -1) {
				const o = t.targets[t.targetIndex];
				this.removeTracedCoordinates_(t.targets[t.targetIndex].startIndex, t.targets[t.targetIndex].endIndex);
			} else for (const o of this.traceSegments_) {
				const a = o[0];
				const l = o[0].geometry;
				const h = o[1];
				const c = o[0].geometry.getCoordinates();
				ra(c, o[0].geometry.getType(), o[0].depth).splice(o[0].index + o[1], 1);
				o[0].geometry.setCoordinates(c);
				if (o[1] === 0) {
					o[0].index -= 1;
				}
			}
			const r = t.targets[i.index];
			this.addTracedCoordinates_(t.targets[i.index], t.targets[i.index].startIndex, i.endIndex);
		} else {
			const r = t.targets[t.targetIndex];
			this.addOrRemoveTracedCoordinates_(t.targets[t.targetIndex], i.endIndex);
		}
		this.traceState_.targetIndex = i.index;
		const s = this.traceState_.targets[this.traceState_.targetIndex];
		this.traceState_.targets[this.traceState_.targetIndex].endIndex = i.endIndex;
	}
	getTraceCandidates_(e) {
		const t = this.getMap(), i = this.pixelTolerance_, s = t.getCoordinateFromPixel([e.pixel[0] - this.pixelTolerance_, e.pixel[1] + this.pixelTolerance_]), r = t.getCoordinateFromPixel([e.pixel[0] + this.pixelTolerance_, e.pixel[1] - this.pixelTolerance_]), o = ye([s, r]);
		return this.traceSource_.getFeaturesInExtent(o);
	}
	toggleTraceState_(e) {
		if (!this.traceSource_ || !this.traceCondition_(e)) return;
		if (this.traceState_.active) {
			this.deactivateTrace_();
			this.traceSegments_ = null;
			return;
		}
		const t = this.getTraceCandidates_(e);
		if (t.length === 0) return;
		const i = Vd(e.coordinate, t);
		if (i.length) {
			this.traceState_ = {
				active: true,
				startCoord: e.coordinate.slice(),
				targets: i,
				targetIndex: -1
			};
		}
	}
	addOrRemoveTracedCoordinates_(e, t) {
		const i = e.startIndex <= e.endIndex, s = e.startIndex <= t;
		i === s ? i && t > e.endIndex || !i && t < e.endIndex ? this.addTracedCoordinates_(e, e.endIndex, t) : (i && t < e.endIndex || !i && t > e.endIndex) && this.removeTracedCoordinates_(t, e.endIndex) : (this.removeTracedCoordinates_(e.startIndex, e.endIndex), this.addTracedCoordinates_(e, e.startIndex, t));
	}
	removeTracedCoordinates_(e, t) {
		let i = 0;
		if (e < t) {
			const s = Math.ceil(e);
			let r = Math.floor(t);
			if (r === t) {
				r -= 1;
			}
			i = r - s + 1;
		} else {
			const s = Math.floor(e);
			let r = Math.ceil(t);
			if (r === t) {
				r += 1;
			}
			i = s - r + 1;
		}
		if (i > 0) for (const s of this.traceSegments_) {
			const r = s[0];
			const o = s[0].geometry;
			const a = s[1];
			let l = s[0].index + 1;
			if (s[1] === 1) {
				l -= i;
			}
			const h = s[0].geometry.getCoordinates();
			ra(h, s[0].geometry.getType(), s[0].depth).splice(l, i);
			s[0].geometry.setCoordinates(h);
			if (s[1] === 1) {
				s[0].index -= i;
			}
		}
	}
	addTracedCoordinates_(e, t, i) {
		const s = [];
		if (t < i) {
			const r = Math.ceil(t);
			let o = Math.floor(i);
			if (o === i) {
				o -= 1;
			}
			for (let a = r; a <= o; ++a) s.push(Ci(e.coordinates, a));
		} else {
			const r = Math.floor(t);
			let o = Math.ceil(i);
			if (o === i) {
				o += 1;
			}
			for (let a = r; a >= o; --a) s.push(Ci(e.coordinates, a));
		}
		if (s.length) for (const r of this.traceSegments_) {
			const o = r[0];
			const a = r[0].geometry;
			const l = r[1];
			const h = r[0].index + 1;
			if (r[1] === 0) {
				s.reverse();
			}
			const c = r[0].geometry.getCoordinates();
			ra(c, r[0].geometry.getType(), r[0].depth).splice(h, 0, ...s);
			r[0].geometry.setCoordinates(c);
			if (r[1] === 1) {
				r[0].index += s.length;
			}
		}
	}
	updateGeometry_(e, t) {
		const i = t[0], s = t[0].depth;
		let r;
		const o = t[0].segment, a = t[0].geometry, l = t[1];
		for (; e.length < t[0].geometry.getStride();) e.push(t[0].segment[t[1]][e.length]);
		switch (t[0].geometry.getType()) {
			case "Point":
				r = e, t[0].segment[0] = e, t[0].segment[1] = e;
				break;
			case "MultiPoint":
				r = t[0].geometry.getCoordinates(), r[t[0].index] = e, t[0].segment[0] = e, t[0].segment[1] = e;
				break;
			case "LineString":
				r = t[0].geometry.getCoordinates(), r[t[0].index + t[1]] = e, t[0].segment[t[1]] = e;
				break;
			case "MultiLineString":
				r = t[0].geometry.getCoordinates(), r[t[0].depth[0]][t[0].index + t[1]] = e, t[0].segment[t[1]] = e;
				break;
			case "Polygon": {
				r = a.getCoordinates();
				const c = r[s[0]];
				const u = i.index + l;
				r[s[0]][u][0] === e[0] && r[s[0]][u][1] === e[1] ? r = null : (r[s[0]][u] = e, u === 0 ? r[s[0]][r[s[0]].length - 1] = e : u === r[s[0]].length - 1 && (r[s[0]][0] = e));
				o[l] = e;
				break;
			}
			case "MultiPolygon": {
				r = a.getCoordinates();
				const c = r[s[1]][s[0]];
				const u = i.index + l;
				r[s[1]][s[0]][u][0] === e[0] && r[s[1]][s[0]][u][1] === e[1] ? r = null : (r[s[1]][s[0]][u] = e, u === 0 ? r[s[1]][s[0]][r[s[1]][s[0]].length - 1] = e : u === r[s[1]][s[0]].length - 1 && (r[s[1]][s[0]][0] = e));
				o[l] = e;
				break;
			}
			case "Circle":
				const h = t[0].geometry;
				if (t[0].segment[0] = e, t[0].segment[1] = e, t[0].index === Oc) {
					this.changingFeature_ = true;
					h.setCenter(e);
					this.changingFeature_ = false;
				} else {
					this.changingFeature_ = true;
					this.getMap().getView().getProjection();
					let c = _n(ue(h.getCenter()), ue(e));
					h.setRadius(c);
					this.changingFeature_ = false;
				}
				break;
		}
		if (r) {
			this.setGeometryCoordinates_(i.geometry, r);
		}
	}
	handleDragEvent(e) {
		this.ignoreNextSingleClick_ = false;
		this.willModifyFeatures_(e, this.dragSegments_.map(([o]) => o));
		const t = [e.coordinate[0] + this.delta_[0], e.coordinate[1] + this.delta_[1]], i = [], s = [], r = this.traceState_.active && !this.traceSegments_ ? this.traceState_.startCoord : null;
		if (r) {
			this.traceSegments_ = [];
			for (const o of this.dragSegments_) {
				const a = o[0];
				if (_n(no(r, o[0].segment), r) / e.map.getView().getResolution() < 1) {
					this.traceSegments_.push(o);
				}
			}
		}
		for (let o = 0, a = this.dragSegments_.length; o < a; ++o) {
			const l = this.dragSegments_[o];
			const h = this.dragSegments_[o][0];
			const c = this.dragSegments_[o][0].feature;
			if (!i.includes(this.dragSegments_[o][0].feature)) {
				i.push(h.feature);
			}
			const u = this.dragSegments_[o][0].geometry;
			if (!s.includes(this.dragSegments_[o][0].geometry)) {
				s.push(this.dragSegments_[o][0].geometry);
			}
			this.updateGeometry_(t, this.dragSegments_[o]);
		}
		this.updateTrace_(e);
		this.createOrUpdateVertexFeature_(t, i, s, true);
	}
	handleDownEvent(e) {
		if (!this.condition_(e)) return false;
		const t = e.coordinate, i = this.findInsertVerticesAndUpdateDragSegments_(e.coordinate);
		if (i != null && i.length && this.insertVertexCondition_(e) && (this.willModifyFeatures_(e, i), this.vertexFeature_)) {
			const s = this.vertexFeature_.getGeometry().getCoordinates();
			for (let r = i.length - 1; r >= 0; --r) this.insertVertex_(i[r], s);
			this.ignoreNextSingleClick_ = true;
		}
		return !!this.vertexFeature_;
	}
	handleUpEvent(e) {
		for (let t = this.dragSegments_.length - 1; t >= 0; --t) {
			const i = this.dragSegments_[t][0];
			const s = this.dragSegments_[t][0].geometry;
			if (this.dragSegments_[t][0].geometry.getType() === "Circle") {
				const r = s;
				const o = s.getCenter();
				const a = i.featureSegments[0];
				const l = i.featureSegments[1];
				i.featureSegments[0].segment[0] = o;
				i.featureSegments[0].segment[1] = o;
				i.featureSegments[1].segment[0] = o;
				i.featureSegments[1].segment[1] = o;
				this.rBush_.update(rs(o), i.featureSegments[0]);
				let h = s;
				this.rBush_.update(h.getExtent(), i.featureSegments[1]);
			} else this.rBush_.update(ye(this.dragSegments_[t][0].segment), this.dragSegments_[t][0]);
		}
		if (this.featuresBeingModified_) {
			this.toggleTraceState_(e);
			this.dispatchEvent(new oa(sa.MODIFYEND, this.featuresBeingModified_, e));
			this.featuresBeingModified_ = null;
		}
		return false;
	}
	handlePointerMove_(e) {
		this.lastCoordinate_ = e.coordinate;
		this.handlePointerAtPixel_(this.lastCoordinate_);
	}
	handlePointerAtPixel_(e) {
		const t = this.getMap(), i = t.getPixelFromCoordinate(e);
		t.getView().getProjection();
		const s = function(a, l) {
			return Nc(e, a) - Nc(e, l);
		};
		let r, o;
		if (this.hitDetection_) {
			const a = typeof this.hitDetection_ == "object" ? (l) => l === this.hitDetection_ : "undefined";
			t.forEachFeatureAtPixel(i, (l, h, c) => {
				if (c && c.getType() === "Point") {
					c = new pl(vi(c.getCoordinates()));
				}
				const u = c || l.getGeometry();
				if (u && u.getType() === "Point" && l instanceof sl && this.features_.includes(l)) {
					o = u;
					const d = l.getGeometry().getFlatCoordinates().slice(0, 2);
					r = [{
						feature: l,
						geometry: o,
						segment: [d, d]
					}];
				}
				return true;
			}, { layerFilter: a });
		}
		if (!r) {
			const a = lt(rs(e, Dc));
			const l = t.getView().getResolution() * this.pixelTolerance_;
			const h = Dn(tt(a, l, Dc));
			r = this.rBush_.getInExtent(h);
		}
		if (r && r.length > 0) {
			const a = r.sort(s)[0];
			const l = r.sort(s)[0].segment;
			let h = kc(e, r.sort(s)[0]);
			const c = t.getPixelFromCoordinate(h);
			let u = _n(i, c);
			if (o || u <= this.pixelTolerance_) {
				const d = {};
				d[O(l)] = true;
				if (!this.snapToPointer_) {
					this.delta_[0] = h[0] - e[0];
					this.delta_[1] = h[1] - e[1];
				}
				if (a.geometry.getType() === "Circle" && a.index === ds) {
					this.snappedToVertex_ = true;
					this.createOrUpdateVertexFeature_(h, [a.feature], [a.geometry], this.snappedToVertex_);
				} else {
					const f = t.getPixelFromCoordinate(l[0]);
					const g = t.getPixelFromCoordinate(l[1]);
					const m = ei(c, f);
					const _ = ei(c, g);
					u = Math.sqrt(Math.min(m, _));
					this.snappedToVertex_ = u <= this.pixelTolerance_;
					if (!this.snappedToVertex_ && !this.insertVertexCondition_(this.lastPointerEvent_)) {
						if (this.vertexFeature_) {
							this.overlay_.getSource().removeFeature(this.vertexFeature_);
							this.vertexFeature_ = null;
						}
						return;
					}
					if (this.snappedToVertex_) {
						h = m > _ ? l[1] : l[0];
					}
					this.createOrUpdateVertexFeature_(h, [a.feature], [a.geometry], this.snappedToVertex_);
					const p = {};
					p[O(a.geometry)] = true;
					for (let y = 1, E = r.length; y < E; ++y) {
						const x = r[y].segment;
						if ($e(l[0], r[y].segment[0]) && $e(l[1], r[y].segment[1]) || $e(l[0], r[y].segment[1]) && $e(l[1], r[y].segment[0])) {
							const T = O(r[y].geometry);
							if (!(T in p)) {
								p[T] = true;
								d[O(x)] = true;
							}
						} else break;
					}
				}
				this.vertexSegments_ = d;
				return;
			}
		}
		if (this.vertexFeature_) {
			this.overlay_.getSource().removeFeature(this.vertexFeature_);
			this.vertexFeature_ = null;
		}
	}
	insertVertex_(e, t) {
		const i = e.segment, s = e.feature, r = e.geometry, o = e.depth, a = e.index;
		let l;
		for (; t.length < e.geometry.getStride();) t.push(0);
		switch (e.geometry.getType()) {
			case "MultiLineString":
				l = e.geometry.getCoordinates(), l[e.depth[0]].splice(e.index + 1, 0, t);
				break;
			case "Polygon":
				l = e.geometry.getCoordinates(), l[e.depth[0]].splice(e.index + 1, 0, t);
				break;
			case "MultiPolygon":
				l = e.geometry.getCoordinates(), l[e.depth[1]][e.depth[0]].splice(e.index + 1, 0, t);
				break;
			case "LineString":
				l = e.geometry.getCoordinates(), l.splice(e.index + 1, 0, t);
				break;
			default: return false;
		}
		this.setGeometryCoordinates_(e.geometry, l);
		const h = this.rBush_;
		this.rBush_.remove(e);
		this.updateSegmentIndices_(e.geometry, e.index, e.depth, 1);
		const c = {
			segment: [e.segment[0], t],
			feature: e.feature,
			geometry: e.geometry,
			depth: e.depth,
			index: e.index
		};
		this.rBush_.insert(ye(c.segment), c);
		this.dragSegments_.push([c, 1]);
		const u = {
			segment: [t, e.segment[1]],
			feature: e.feature,
			geometry: e.geometry,
			depth: e.depth,
			index: e.index + 1
		};
		this.rBush_.insert(ye(u.segment), u);
		this.dragSegments_.push([u, 0]);
		return true;
	}
	updatePointer_(e) {
		var t;
		if (e) {
			this.findInsertVerticesAndUpdateDragSegments_(e);
		}
		return (t = this.vertexFeature_) == null ? "undefined" : t.getGeometry().getCoordinates();
	}
	getPoint() {
		var t;
		const e = (t = this.vertexFeature_) == null ? "undefined" : t.getGeometry().getCoordinates();
		return e ? vi(e, this.getMap().getView().getProjection()) : null;
	}
	canRemovePoint() {
		if (!this.vertexFeature_ || this.vertexFeature_.get("geometries").every((i) => )) return false;
		const e = this.vertexFeature_.getGeometry().getCoordinates();
		return this.rBush_.getInExtent(ye([e])).some(({ segment: i }) => );
	}
	removePoint(e) {
		if (e) {
			e = ue(e, this.getMap().getView().getProjection());
			this.updatePointer_(e);
		}
		if (!this.lastPointerEvent_ || this.lastPointerEvent_ && this.lastPointerEvent_.type != Q.POINTERDRAG) {
			const t = this.lastPointerEvent_;
			this.willModifyFeatures_(this.lastPointerEvent_, this.dragSegments_.map(([s]) => s));
			const i = this.removeVertex_();
			if (this.featuresBeingModified_) {
				this.dispatchEvent(new oa(sa.MODIFYEND, this.featuresBeingModified_, this.lastPointerEvent_));
			}
			this.featuresBeingModified_ = null;
			return i;
		}
		return false;
	}
	removeVertex_() {
		const e = this.dragSegments_, t = {};
		let i = false, s, r, o, a, l, h, c, u, d, f, g;
		for (l = this.dragSegments_.length - 1; l >= 0; --l) {
			o = this.dragSegments_[l];
			f = o[0];
			g = O(f.feature);
			if (f.depth) {
				g += "-" + f.depth.join("-");
			}
			if (!(g in t)) {
				t[g] = {};
			}
			o[1] === 0 ? (t[g].right = f, t[g].index = f.index) : o[1] == 1 && (t[g].left = f, t[g].index = f.index + 1);
		}
		for (g in t) {
			switch (d = t[g].right, c = t[g].left, h = t[g].index, u = h - 1, c !== "undefined" ? f = c : f = d, u < 0 && (u = 0), a = f.geometry, r = a.getCoordinates(), s = r, i = false, a.getType()) {
				case "MultiLineString":
					r[f.depth[0]].length > 2 && (r[f.depth[0]].splice(h, 1), i = true);
					break;
				case "LineString":
					r.length > 2 && (r.splice(h, 1), i = true);
					break;
				case "MultiPolygon": s = s[f.depth[1]];
				case "Polygon":
					s = s[f.depth[0]], s.length > 4 && (h == s.length - 1 && (h = 0), s.splice(h, 1), i = true, h === 0 && (s.pop(), s.push(s[0]), u = s.length - 1));
					break;
			}
		}
		return i;
	}
	canInsertPoint() {
		if (!this.vertexFeature_ || this.vertexFeature_.get("geometries").every((i) => )) return false;
		const e = this.vertexFeature_.getGeometry().getCoordinates();
		return this.rBush_.getInExtent(ye([e])).some(({ segment: i }) => !($e(i[0], e) || $e(i[1], e)));
	}
	insertPoint(e) {
		var s;
		const t = e ? ue(e, this.getMap().getView().getProjection()) : (s = this.vertexFeature_) == null ? "undefined" : s.getGeometry().getCoordinates();
		return t ? this.findInsertVerticesAndUpdateDragSegments_(t).reduce((r, o) => , false) : false;
	}
	setGeometryCoordinates_(e, t) {
		this.changingFeature_ = true;
		e.setCoordinates(t);
		this.changingFeature_ = false;
	}
	updateSegmentIndices_(e, t, i, s) {
		this.rBush_.forEachInExtent(e.getExtent(), function(r) {
			if (r.geometry === e && (i === "undefined" || r.depth === "undefined" || kt(r.depth, i)) && r.index > t) {
				r.index += s;
			}
		});
	}
	disposeInternal() {
		super.disposeInternal();
		if (this.featuresCollection_) {
			this.featuresCollection_.removeEventListener(_e.ADD, this.handleExternalCollectionAdd_);
			this.featuresCollection_.removeEventListener(_e.REMOVE, this.handleExternalCollectionRemove_);
			for (const e of this.featuresCollection_.getArray()) {
				e.removeEventListener(U.CHANGE, this.handleFeatureChange_);
				if (this.filterFunctionWasSupplied_) {
					e.removeEventListener(xt.PROPERTYCHANGE, this.handleFeatureChange_);
				}
			}
		} else if (this.source_) {
			this.source_.removeEventListener(Te.ADDFEATURE, this.handleSourceAdd_);
			this.source_.removeEventListener(Te.REMOVEFEATURE, this.handleSourceRemove_);
			for (const e of this.source_.getFeatures()) {
				e.removeEventListener(U.CHANGE, this.handleFeatureChange_);
				if (this.filterFunctionWasSupplied_) {
					e.removeEventListener(xt.PROPERTYCHANGE, this.handleFeatureChange_);
				}
			}
		}
	}
}
function JE(n, e) {
	return n.index - e.index;
}
function Nc(n, e, t) {
	const i = e.geometry;
	if (e.geometry.getType() === "Circle") {
		let r = i;
		if (e.index === ds) {
			const o = ei(r.getCenter(), ue(n));
			const a = Math.sqrt(o) - r.getRadius();
			return a * a;
		}
	}
	const s = ue(n);
	Rn[0] = ue(e.segment[0]);
	Rn[1] = ue(e.segment[1]);
	return Wf(s, Rn);
}
function kc(n, e, t) {
	const i = e.geometry;
	if (e.geometry.getType() === "Circle" && e.index === ds) return vi(e.geometry.getClosestPoint(ue(n)));
	const s = ue(n);
	Rn[0] = ue(e.segment[0]);
	Rn[1] = ue(e.segment[1]);
	return vi(no(s, Rn));
}
function QE() {
	const n = bl();
	return function(e, t) {
		return n.Point;
	};
}
const _r = { SELECT: "select" };
class mr extends wf {
	constructor(e, t, i, s) {
		super(e);
		this.selected = t;
		this.deselected = i;
		this.mapBrowserEvent = s;
	}
}
const pr = {};
class lh extends Jp {
	constructor(e) {
		super();
		this.on;
		this.once;
		this.un;
		e = e || {};
		this.boundAddFeature_ = this.addFeature_.bind(this);
		this.boundRemoveFeature_ = this.removeFeature_.bind(this);
		this.condition_ = e.condition ? e.condition : _d;
		this.addCondition_ = e.addCondition ? e.addCondition : Xi;
		this.removeCondition_ = e.removeCondition ? e.removeCondition : Xi;
		this.toggleCondition_ = e.toggleCondition ? e.toggleCondition : Zr;
		this.multi_ = e.multi ? e.multi : false;
		this.filter_ = e.filter ? e.filter : ni;
		this.hitTolerance_ = e.hitTolerance ? e.hitTolerance : 0;
		this.style_ = e.style !== "undefined" ? e.style : eT();
		this.features_ = e.features || new Af();
		let t;
		if (e.layers) if (typeof e.layers == "function") t = e.layers;
		else {
			const i = e.layers;
			t = function(s) {
				return i.includes(s);
			};
		}
		else t = ni;
		this.layerFilter_ = t;
		this.featureLayerAssociation_ = {};
	}
	addFeatureLayerAssociation_(e, t) {
		this.featureLayerAssociation_[O(e)] = t;
	}
	getFeatures() {
		return this.features_;
	}
	getHitTolerance() {
		return this.hitTolerance_;
	}
	getLayer(e) {
		return this.featureLayerAssociation_[O(e)];
	}
	setHitTolerance(e) {
		this.hitTolerance_ = e;
	}
	setMap(e) {
		if (this.getMap() && this.style_) {
			this.features_.forEach(this.restorePreviousStyle_.bind(this));
		}
		super.setMap(e);
		e ? (this.features_.addEventListener(_e.ADD, this.boundAddFeature_), this.features_.addEventListener(_e.REMOVE, this.boundRemoveFeature_), this.style_ && this.features_.forEach(this.applySelectedStyle_.bind(this))) : (this.features_.removeEventListener(_e.ADD, this.boundAddFeature_), this.features_.removeEventListener(_e.REMOVE, this.boundRemoveFeature_));
	}
	addFeature_(e) {
		const t = e.element;
		if (this.style_) {
			this.applySelectedStyle_(e.element);
		}
		if (!this.getLayer(e.element)) {
			const i = this.findLayerOfFeature_(t);
			if (i) {
				this.addFeatureLayerAssociation_(t, i);
			}
		}
	}
	removeFeature_(e) {
		if (this.style_) {
			this.restorePreviousStyle_(e.element);
		}
		this.removeFeatureLayerAssociation_(e.element);
	}
	findLayerOfFeature_(e) {
		return this.getMap().getAllLayers().find(function(i) {
			if (i instanceof Fp && i.getSource() && i.getSource().hasFeature(e)) return i;
		});
	}
	getStyle() {
		return this.style_;
	}
	applySelectedStyle_(e) {
		const t = O(e);
		if (!(t in pr)) {
			pr[t] = e.getStyle();
		}
		e.setStyle(this.style_);
	}
	restorePreviousStyle_(e) {
		const t = this.getMap().getInteractions().getArray();
		for (let s = t.length - 1; s >= 0; --s) {
			const r = t[s];
			if (t[s] !== this && t[s] instanceof lh && t[s].getStyle() && t[s].getFeatures().getArray().lastIndexOf(e) !== -1) {
				e.setStyle(r.getStyle());
				return;
			}
		}
		const i = O(e);
		e.setStyle(pr[i]);
		delete pr[i];
	}
	removeFeatureLayerAssociation_(e) {
		delete this.featureLayerAssociation_[O(e)];
	}
	selectFeature(e) {
		const t = this.findLayerOfFeature_(e);
		if (!this.layerFilter_(t) || !this.filter_(e, t)) return false;
		const i = this.getFeatures();
		return i.getArray().includes(e) ? false : (this.addFeatureLayerAssociation_(e, t), i.push(e), this.dispatchEvent(new mr(_r.SELECT, [e], [], "undefined")), true);
	}
	deselectFeature(e) {
		const t = this.getFeatures(), i = t.getArray().indexOf(e);
		return i === -1 ? false : (t.removeAt(i), this.dispatchEvent(new mr(_r.SELECT, [], [e], "undefined")), true);
	}
	toggleFeature(e) {
		if (!this.deselectFeature(e)) {
			this.selectFeature(e);
		}
	}
	clearSelection() {
		const e = this.getFeatures();
		if (e.getLength() !== 0) {
			const t = e.getArray().slice();
			e.clear();
			this.dispatchEvent(new mr(_r.SELECT, [], t, "undefined"));
		}
	}
	handleEvent(e) {
		if (!this.condition_(e)) return true;
		const t = this.addCondition_(e), i = this.removeCondition_(e), s = this.toggleCondition_(e), r = !t && !i && !s, o = e.map, a = this.getFeatures(), l = [], h = [];
		if (r) {
			o.forEachFeatureAtPixel(e.pixel, (c, u) => {
				if (!(!(c instanceof sl) || !this.filter_(c, u))) return this.addFeatureLayerAssociation_(c, u), h.push(c), !this.multi_;
			}, {
				layerFilter: this.layerFilter_,
				hitTolerance: this.hitTolerance_
			});
			for (let c = a.getLength() - 1; c >= 0; --c) {
				const u = a.item(c);
				const d = h.indexOf(u);
				d === -1 ? (a.removeAt(c), l.push(u)) : h.splice(d, 1);
			}
			if (h.length !== 0) {
				a.extend(h);
			}
		} else {
			o.forEachFeatureAtPixel(e.pixel, (c, u) => {
				if (!(c instanceof sl) || !this.filter_(c, u)) return;
				const d = a.getArray().includes(c);
				d && (i || s) ? l.push(c) : !d && (t || s) && (this.addFeatureLayerAssociation_(c, u), h.push(c));
				return !this.multi_;
			}, {
				layerFilter: this.layerFilter_,
				hitTolerance: this.hitTolerance_
			});
			for (let c = l.length - 1; c >= 0; --c) a.remove(l[c]);
			a.extend(h);
		}
		if (h.length > 0 || l.length > 0) {
			this.dispatchEvent(new mr(_r.SELECT, h, l, e));
		}
		return true;
	}
}
function eT() {
	const n = bl();
	dt(n.Polygon, n.LineString);
	dt(n.GeometryCollection, n.LineString);
	return function(e) {
		return e.getGeometry() ? n[e.getGeometry().getType()] : null;
	};
}
const aa = {
	SNAP: "snap",
	UNSNAP: "unsnap"
};
class la extends wf {
	constructor(e, t) {
		super(e);
		this.vertex = t.vertex;
		this.vertexPixel = t.vertexPixel;
		this.feature = t.feature;
		this.segment = t.segment;
	}
}
const Kd = {
	Circle(n, e) {
		const i = Jg(n);
		return Kd.Polygon(i);
	},
	GeometryCollection(n, e) {
		const t = [], i = n.getGeometriesArray();
		for (let s = 0; s < i.length; ++s) {
			const r = this[i[s].getType()];
			if (this[i[s].getType()]) {
				t.push(this[i[s].getType()](i[s], e));
			}
		}
		return t.flat();
	},
	LineString(n) {
		const e = [], t = n.getFlatCoordinates(), i = n.getStride();
		for (let s = 0, r = t.length - i; s < r; s += i) e.push([t.slice(s, s + 2), t.slice(s + i, s + i + 2)]);
		return e;
	},
	MultiLineString(n) {
		const e = [], t = n.getFlatCoordinates(), i = n.getStride(), s = n.getEnds();
		let r = 0;
		for (let o = 0, a = s.length; o < a; ++o) {
			const l = s[o];
			for (let h = r, c = s[o] - i; h < c; h += i) e.push([t.slice(h, h + 2), t.slice(h + i, h + i + 2)]);
			r = s[o];
		}
		return e;
	},
	MultiPoint(n) {
		const e = [], t = n.getFlatCoordinates(), i = n.getStride();
		for (let s = 0, r = t.length; s < r; s += i) e.push([t.slice(s, s + 2)]);
		return e;
	},
	MultiPolygon(n) {
		const e = [], t = n.getFlatCoordinates(), i = n.getStride(), s = n.getEndss();
		let r = 0;
		for (let o = 0, a = s.length; o < a; ++o) {
			const l = s[o];
			for (let h = 0, c = s[o].length; h < c; ++h) {
				const u = l[h];
				for (let d = r, f = l[h] - i; d < f; d += i) e.push([t.slice(d, d + 2), t.slice(d + i, d + i + 2)]);
				r = l[h];
			}
		}
		return e;
	},
	Point(n) {
		return [[n.getFlatCoordinates().slice(0, 2)]];
	},
	Polygon(n) {
		const e = [], t = n.getFlatCoordinates(), i = n.getStride(), s = n.getEnds();
		let r = 0;
		for (let o = 0, a = s.length; o < a; ++o) {
			const l = s[o];
			for (let h = r, c = s[o] - i; h < c; h += i) e.push([t.slice(h, h + 2), t.slice(h + i, h + i + 2)]);
			r = s[o];
		}
		return e;
	}
};
function Gc(n) {
	return n.feature ? n.feature : n.element ? n.element : null;
}
const ha = [];
const Xt = [];
const on = [];
class tT extends oy {
	constructor(e) {
		e = e || {};
		super({
			handleDownEvent: ni,
			stopDown: Xi
		});
		this.on;
		this.once;
		this.un;
		this.source_ = e.source ? e.source : null;
		this.vertex_ = e.vertex !== "undefined" ? e.vertex : true;
		this.edge_ = e.edge !== "undefined" ? e.edge : true;
		this.intersection_ = e.intersection !== "undefined" ? e.intersection : false;
		this.features_ = e.features ? e.features : null;
		this.featuresListenerKeys_ = [];
		this.featureChangeListenerKeys_ = {};
		this.indexedFeaturesExtents_ = {};
		this.pendingFeatures_ = {};
		this.pixelTolerance_ = e.pixelTolerance !== "undefined" ? e.pixelTolerance : 10;
		this.rBush_ = new o_();
		this.snapped_ = null;
		this.segmenters_ = Object.assign({}, Kd, e.segmenters);
	}
	addFeature(e, t) {
		t = t !== "undefined" ? t : true;
		const i = O(e), s = e.getGeometry();
		if (s) {
			const r = this.segmenters_[s.getType()];
			if (this.segmenters_[s.getType()]) {
				this.indexedFeaturesExtents_[i] = s.getExtent(je());
				const o = r.call(this.segmenters_, s, this.getMap().getView().getProjection());
				let a = o.length;
				for (let l = 0; l < a; ++l) {
					const h = o[l];
					Xt[l] = ye(o[l]);
					on[l] = {
						feature: e,
						segment: o[l]
					};
				}
				if (this.intersection_) for (let l = 0, h = o.length; l < h; ++l) {
					const c = o[l];
					if (o[l].length === 1) continue;
					const u = Xt[l];
					for (let f = 0, g = l - 1; f < g; ++f) {
						const m = o[f];
						if (!me(u, Xt[f])) continue;
						const _ = Yh(c, o[f]);
						if (!_) continue;
						const p = [_];
						Xt[a] = ye(p);
						on[a++] = {
							feature: e,
							intersectionFeature: e,
							segment: p
						};
					}
					const d = this.rBush_.getInExtent(Xt[l]);
					for (let f = 0, g = d.length; f < g; ++f) {
						const m = d[f].segment;
						if (d[f].segment.length === 1) continue;
						const _ = Yh(c, d[f].segment);
						if (!_) continue;
						const p = [_];
						Xt[a] = ye(p);
						on[a++] = {
							feature: e,
							intersectionFeature: d[f].feature,
							segment: p
						};
					}
				}
				a === 1 ? this.rBush_.insert(Xt[0], on[0]) : (Xt.length = a, on.length = a, this.rBush_.load(Xt, on));
			}
		}
		if (t) {
			if (this.featureChangeListenerKeys_[i]) {
				se(this.featureChangeListenerKeys_[i]);
			}
			this.featureChangeListenerKeys_[i] = Z(e, U.CHANGE, this.handleFeatureChange_, this);
		}
	}
	getFeatures_() {
		let e;
		this.features_ ? e = this.features_ : this.source_ && (e = this.source_.getFeatures());
		return e;
	}
	areSnapDataEqual_(e, t) {
		return e.segment === t.segment && e.feature === t.feature;
	}
	handleEvent(e) {
		const t = this.snapTo(e.pixel, e.coordinate, e.map);
		t ? (e.coordinate = t.vertex.slice(0, 2), e.pixel = t.vertexPixel, this.snapped_ && !this.areSnapDataEqual_(this.snapped_, t) && this.dispatchEvent(new la(aa.UNSNAP, this.snapped_)), this.snapped_ = {
			vertex: e.coordinate,
			vertexPixel: e.pixel,
			feature: t.feature,
			segment: t.segment
		}, this.dispatchEvent(new la(aa.SNAP, this.snapped_))) : this.snapped_ && (this.dispatchEvent(new la(aa.UNSNAP, this.snapped_)), this.snapped_ = null);
		return super.handleEvent(e);
	}
	handleFeatureAdd_(e) {
		const t = Gc(e);
		if (t) {
			this.addFeature(t);
		}
	}
	handleFeatureRemove_(e) {
		const t = Gc(e);
		if (t) {
			this.removeFeature(t);
			delete this.pendingFeatures_[O(t)];
		}
	}
	handleFeatureChange_(e) {
		const t = e.target;
		this.handlingDownUpSequence ? this.pendingFeatures_[O(e.target)] = e.target : this.updateFeature_(e.target);
	}
	handleUpEvent(e) {
		const t = Object.values(this.pendingFeatures_);
		if (t.length) {
			for (const i of t) this.updateFeature_(i);
			Wi(this.pendingFeatures_);
		}
		return false;
	}
	removeFeature(e, t) {
		const i = t !== "undefined" ? t : true, s = O(e), r = this.indexedFeaturesExtents_[s];
		if (this.indexedFeaturesExtents_[s]) {
			const o = this.rBush_;
			this.rBush_.getInExtent(r).forEach((a) => {
				if (e === a.feature || e === a.intersectionFeature) {
					o.remove(a);
				}
			});
		}
		if (i) {
			se(this.featureChangeListenerKeys_[s]);
			delete this.featureChangeListenerKeys_[s];
		}
	}
	setMap(e) {
		const t = this.getMap(), i = this.featuresListenerKeys_;
		let s = this.getFeatures_();
		if (!Array.isArray(s)) {
			s = s.getArray();
		}
		if (t) {
			this.featuresListenerKeys_.forEach(se);
			this.featuresListenerKeys_.length = 0;
			this.rBush_.clear();
			Object.values(this.featureChangeListenerKeys_).forEach(se);
			this.featureChangeListenerKeys_ = {};
		}
		super.setMap(e);
		if (e) {
			this.features_ ? i.push(Z(this.features_, _e.ADD, this.handleFeatureAdd_, this), Z(this.features_, _e.REMOVE, this.handleFeatureRemove_, this)) : this.source_ && i.push(Z(this.source_, Te.ADDFEATURE, this.handleFeatureAdd_, this), Z(this.source_, Te.REMOVEFEATURE, this.handleFeatureRemove_, this));
			for (const r of s) this.addFeature(r);
		}
	}
	snapTo(e, t, i) {
		i.getView().getProjection();
		const s = ue(t), r = Dn(tt(ye([s]), i.getView().getResolution() * this.pixelTolerance_)), o = this.rBush_.getInExtent(r), a = o.length;
		if (o.length === 0) return null;
		let l, h = null, c, u = null;
		const d = this.pixelTolerance_ * this.pixelTolerance_, f = () => {
			if (!l) return null;
			const g = i.getPixelFromCoordinate(l);
			return ei(e, g) > d ? null : {
				vertex: l,
				vertexPixel: [Math.round(g[0]), Math.round(g[1])],
				feature: c,
				segment: u
			};
		};
		if (this.vertex_ || this.intersection_) {
			for (let m = 0; m < a; ++m) {
				const _ = o[m];
				if (o[m].feature.getGeometry().getType() !== "Circle") for (const p of o[m].segment) {
					const y = ue(p);
					const E = ei(s, y);
					if (E < h && (this.intersection_ && _.intersectionFeature || this.vertex_ && !_.intersectionFeature)) {
						l = p;
						h = E;
						c = _.feature;
					}
				}
			}
			const g = f();
			if (g) return g;
		}
		if (this.edge_) {
			for (let m = 0; m < a; ++m) {
				let _ = null;
				const p = o[m];
				if (o[m].feature.getGeometry().getType() === "Circle") {
					let y = p.feature.getGeometry();
					_ = zf(s, y);
				} else {
					const [y, E] = p.segment;
					if (E) {
						ha[0] = ue(y);
						ha[1] = ue(E);
						_ = no(s, ha);
					}
				}
			}
			const g = f();
			if (g) return g;
		}
		return null;
	}
	updateFeature_(e) {
		this.removeFeature(e, false);
		this.addFeature(e, false);
	}
}
function Hd() {
	return [
		1,
		0,
		0,
		0,
		0,
		1,
		0,
		0,
		0,
		0,
		1,
		0,
		0,
		0,
		0,
		1
	];
}
function za(n, e) {
	n[0] = e[0];
	n[1] = e[1];
	n[4] = e[2];
	n[5] = e[3];
	n[12] = e[4];
	n[13] = e[5];
	return n;
}
function Xa(n) {
	return Array.isArray(n) ? Math.min(...n) : n;
}
class iT extends S_ {
	constructor(e, t, i, s, r, o, a) {
		let l = e.getExtent();
		if (l && e.canWrapX()) {
			l = l.slice();
			l[0] = null;
			l[2] = null;
		}
		let h = t.getExtent();
		if (h && t.canWrapX()) {
			h = h.slice();
			h[0] = null;
			h[2] = null;
		}
		const c = h ? Et(i, h) : i, u = Ot(c), d = ws(e, t, u, s), f = Nd, g = new xE(e, t, c, l, d * Nd, s), m = g.calculateSourceExtent(), _ = Pi(m) ? null : o(m, d, r), p = _ ? $.IDLE : $.EMPTY, y = _ ? _.getPixelRatio() : 1;
		super(i, s, y, p);
		this.targetProj_ = t;
		this.maxSourceExtent_ = l;
		this.triangulation_ = g;
		this.targetResolution_ = s;
		this.targetExtent_ = i;
		this.sourceImage_ = _;
		this.sourcePixelRatio_ = y;
		this.interpolate_ = a;
		this.canvas_ = null;
		this.sourceListenerKey_ = null;
	}
	disposeInternal() {
		if (this.state == $.LOADING) {
			this.unlistenSource_();
		}
		super.disposeInternal();
	}
	getImage() {
		return this.canvas_;
	}
	getProjection() {
		return this.targetProj_;
	}
	reproject_() {
		const e = this.sourceImage_.getState();
		if (e == $.LOADED) {
			const t = J(this.targetExtent_) / this.targetResolution_;
			const i = Ce(this.targetExtent_) / this.targetResolution_;
			this.canvas_ = Od(t, i, this.sourcePixelRatio_, Xa(this.sourceImage_.getResolution()), this.maxSourceExtent_, this.targetResolution_, this.targetExtent_, this.triangulation_, [{
				extent: this.sourceImage_.getExtent(),
				image: this.sourceImage_.getImage()
			}], 0, "undefined", this.interpolate_, true);
		}
		this.state = e;
		this.changed();
	}
	load() {
		if (this.state == $.IDLE) {
			this.state = $.LOADING;
			this.changed();
			const e = this.sourceImage_.getState();
			e == $.LOADED || e == $.ERROR ? this.reproject_() : (this.sourceListenerKey_ = Z(this.sourceImage_, U.CHANGE, (t) => {
				const i = this.sourceImage_.getState();
				if (i == $.LOADED || i == $.ERROR) {
					this.unlistenSource_();
					this.reproject_();
				}
			}), this.sourceImage_.load());
		}
	}
	unlistenSource_() {
		se(this.sourceListenerKey_);
		this.sourceListenerKey_ = null;
	}
}
const Ft = 4;
const ca = {
	IMAGELOADSTART: "imageloadstart",
	IMAGELOADEND: "imageloadend",
	IMAGELOADERROR: "imageloaderror"
};
class sT extends wf {
	constructor(e, t) {
		super(e);
		this.image = t;
	}
}
class rT extends a_ {
	constructor(e) {
		super({
			attributions: e.attributions,
			projection: e.projection,
			state: e.state,
			interpolate: e.interpolate !== "undefined" ? e.interpolate : true
		});
		this.on;
		this.once;
		this.un;
		this.loader = e.loader || null;
		this.resolutions_ = e.resolutions !== "undefined" ? e.resolutions : null;
		this.reprojectedImage_ = null;
		this.reprojectedRevision_ = 0;
		this.image = null;
		this.wantedExtent_;
		this.wantedResolution_;
		this.static_ = e.loader ? e.loader.length === 0 : false;
		this.wantedProjection_ = null;
	}
	getResolutions() {
		return this.resolutions_;
	}
	setResolutions(e) {
		this.resolutions_ = e;
	}
	findNearestResolution(e) {
		const t = this.getResolutions();
		if (t) {
			const i = io(t, e, 0);
			e = t[i];
		}
		return e;
	}
	getImage(e, t, i, s) {
		const r = this.getProjection();
		if (!r || !s || Ae(r, s)) return r && (s = r), this.getImageInternal(e, t, i, s);
		if (this.reprojectedImage_) {
			if (this.reprojectedRevision_ == this.getRevision() && Ae(this.reprojectedImage_.getProjection(), s) && this.reprojectedImage_.getResolution() == t && Si(this.reprojectedImage_.getExtent(), e)) return this.reprojectedImage_;
			this.reprojectedImage_.dispose();
			this.reprojectedImage_ = null;
		}
		this.reprojectedImage_ = new iT(r, s, e, t, i, (o, a, l) => this.getImageInternal(o, a, l, r), this.getInterpolate());
		this.reprojectedRevision_ = this.getRevision();
		return this.reprojectedImage_;
	}
	getImageInternal(e, t, i, s) {
		if (this.loader) {
			const r = ch(e, t, i, 1);
			const o = this.findNearestResolution(t);
			if (this.image && (this.static_ || this.wantedProjection_ === s && (this.wantedExtent_ && at(this.wantedExtent_, r) || at(this.image.getExtent(), r)) && (this.wantedResolution_ && Xa(this.wantedResolution_) === o || Xa(this.image.getResolution()) === o))) return this.image;
			this.wantedProjection_ = s;
			this.wantedExtent_ = r;
			this.wantedResolution_ = o;
			this.image = new S_(r, o, i, this.loader);
			this.image.addEventListener(U.CHANGE, this.handleImageChange.bind(this));
		}
		return this.image;
	}
	handleImageChange(e) {
		const t = e.target;
		let i;
		switch (e.target.getState()) {
			case $.LOADING:
				this.loading = true, i = ca.IMAGELOADSTART;
				break;
			case $.LOADED:
				this.loading = false, i = ca.IMAGELOADEND;
				break;
			case $.ERROR:
				this.loading = false, i = ca.IMAGELOADERROR;
				break;
			default: return;
		}
		if (this.hasListener(i)) {
			this.dispatchEvent(new sT(i, e.target));
		}
	}
}
function hh(n, e) {
	n.getImage().src = e;
}
function ch(n, e, t, i) {
	const s = e / t, r = Ot(n), o = _i(J(n) / s, Ft), a = _i(Ce(n) / s, Ft), l = _i((i - 1) * o / 2, Ft), h = o + 2 * l, c = _i((i - 1) * a / 2, Ft), u = a + 2 * c;
	return gs(r, s, 0, [h, u]);
}
function oT(n, e, t, i, s, r) {
	const o = s.getCode().split(/:(?=\d+$)/).pop(), a = t / i, l = [Pr(J(e) / a, Ft), Pr(Ce(e) / a, Ft)];
	r.SIZE = l[0] + "," + l[1];
	r.BBOX = e.join(",");
	r.BBOXSR = o;
	r.IMAGESR = o;
	r.DPI = Math.round(r.DPI ? r.DPI * i : 90 * i);
	const h = n.replace(/MapServer\/?$/, "MapServer/export").replace(/ImageServer\/?$/, "ImageServer/exportImage");
	return An(h, r);
}
function aT(n) {
	var o, a;
	const e = n.load ? n.load : Bn, t = H(n.projection || "EPSG:3857"), i = (o = n.ratio) != null ? o : 1.5, s = (a = n.crossOrigin) != null ? a : null, r = n.referrerPolicy;
	return function(l, h, c) {
		c = n.hidpi ? c : 1;
		const u = {
			F: "image",
			FORMAT: "PNG32",
			TRANSPARENT: true
		};
		Object.assign(u, n.params);
		l = ch(l, h, c, i);
		const d = oT(n.url, l, h, c, t, u), f = new Image();
		f.crossOrigin = s;
		if (r !== "undefined") {
			f.referrerPolicy = r;
		}
		return e(f, d).then((g) => {
			const m = J(l) / g.width * c;
			return {
				image: g,
				extent: l,
				resolution: m,
				pixelRatio: c
			};
		});
	};
}
class lT extends rT {
	constructor(e) {
		e = e || {};
		super({
			attributions: e.attributions,
			interpolate: e.interpolate,
			projection: e.projection,
			resolutions: e.resolutions
		});
		this.crossOrigin_ = e.crossOrigin !== "undefined" ? e.crossOrigin : null;
		this.referrerPolicy_ = e.referrerPolicy;
		this.hidpi_ = e.hidpi !== "undefined" ? e.hidpi : true;
		this.url_ = e.url;
		this.imageLoadFunction_ = e.imageLoadFunction !== "undefined" ? e.imageLoadFunction : hh;
		this.params_ = Object.assign({}, e.params);
		this.imageSize_ = [0, 0];
		this.renderedRevision_ = 0;
		this.ratio_ = e.ratio !== "undefined" ? e.ratio : 1.5;
		this.loaderProjection_ = null;
	}
	getParams() {
		return this.params_;
	}
	getImageInternal(e, t, i, s) {
		return this.url_ === "undefined" ? null : ((!this.loader || this.loaderProjection_ !== s) && (this.loaderProjection_ = s, this.loader = aT({
			crossOrigin: this.crossOrigin_,
			referrerPolicy: this.referrerPolicy_,
			params: this.params_,
			projection: s,
			hidpi: this.hidpi_,
			url: this.url_,
			ratio: this.ratio_,
			load: (r, o) => this.image.setImage(r)
		})), super.getImageInternal(e, t, i, s));
	}
	getImageLoadFunction() {
		return this.imageLoadFunction_;
	}
	getUrl() {
		return this.url_;
	}
	setImageLoadFunction(e) {
		this.imageLoadFunction_ = e;
		this.changed();
	}
	setUrl(e) {
		if (e != this.url_) {
			this.url_ = e;
			this.loader = null;
			this.changed();
		}
	}
	setParams(e) {
		this.params_ = Object.assign({}, e);
		this.changed();
	}
	updateParams(e) {
		Object.assign(this.params_, e);
		this.changed();
	}
	changed() {
		this.image = null;
		super.changed();
	}
}
function hT(n) {
	var s;
	const e = n.load || Bn, t = n.imageExtent, i = (s = n.crossOrigin) != null ? s : null;
	return () => {
		const r = new Image();
		r.crossOrigin = i;
		if (n.referrerPolicy !== "undefined") {
			r.referrerPolicy = n.referrerPolicy;
		}
		return e(r, n.url).then((o) => {
			const a = J(t) / o.width, l = Ce(t) / o.height;
			return {
				image: o,
				extent: t,
				resolution: a !== l ? [a, l] : l,
				pixelRatio: 1
			};
		});
	};
}
class cT extends rT {
	constructor(e) {
		const t = e.crossOrigin !== "undefined" ? e.crossOrigin : null, i = e.imageLoadFunction !== "undefined" ? e.imageLoadFunction : hh;
		super({
			attributions: e.attributions,
			interpolate: e.interpolate,
			projection: H(e.projection)
		});
		this.url_ = e.url;
		this.imageExtent_ = e.imageExtent;
		this.image = null;
		this.image = new S_(this.imageExtent_, "undefined", 1, hT({
			url: e.url,
			imageExtent: e.imageExtent,
			crossOrigin: t,
			referrerPolicy: e.referrerPolicy,
			load: (s, r) => this.image.setImage(s)
		}));
		this.image.addEventListener(U.CHANGE, this.handleImageChange.bind(this));
	}
	getImageExtent() {
		return this.imageExtent_;
	}
	getImageInternal(e, t, i, s) {
		return me(e, this.image.getExtent()) ? this.image : null;
	}
	getUrl() {
		return this.url_;
	}
}
const Hr = "1.3.0";
const Bc = [101, 101];
function qd(n, e, t, i, s) {
	s.WIDTH = t[0];
	s.HEIGHT = t[1];
	const r = i.getAxisOrientation(), o = Ha(s.VERSION, "1.3") >= 0;
	s[o ? "CRS" : "SRS"] = i.getCode();
	const a = o && r.startsWith("ne") ? [
		e[1],
		e[0],
		e[3],
		e[2]
	] : e;
	s.BBOX = a.join(",");
	return An(n, s);
}
function Jd(n, e, t, i, s, r, o) {
	r = Object.assign({ REQUEST: "GetMap" }, r);
	const a = e / t, l = [Pr(J(n) / a, Ft), Pr(Ce(n) / a, Ft)];
	switch (o) {
		case "geoserver":
			const c = 90 * t + .5 | 0;
			"FORMAT_OPTIONS" in r ? r.FORMAT_OPTIONS += ";dpi:" + c : r.FORMAT_OPTIONS = "dpi:" + c;
			break;
		case "mapserver":
			r.MAP_RESOLUTION = 90 * t;
			break;
		case "carmentaserver":
		case "qgis":
			r.DPI = 90 * t;
			break;
		default: throw new Error("Unknown `serverType` configured");
	}
	return qd(s, n, l, i, r);
}
function qr(n, e) {
	return Object.assign({
		REQUEST: e,
		SERVICE: "WMS",
		VERSION: Hr,
		FORMAT: "image/png",
		STYLES: "",
		TRANSPARENT: "TRUE"
	}, n);
}
function uT(n) {
	var a;
	const e = n.hidpi === "undefined" ? true : n.hidpi, t = H(n.projection || "EPSG:3857"), i = n.ratio || 1.5, s = n.load || Bn, r = (a = n.crossOrigin) != null ? a : null, o = n.referrerPolicy;
	return (l, h, c) => {
		l = ch(l, h, c, i);
		if (c != 1 && (!e || n.serverType === "undefined")) {
			c = 1;
		}
		const u = Jd(l, h, c, t, n.url, qr(n.params, "GetMap"), n.serverType), d = new Image();
		d.crossOrigin = r;
		if (o !== "undefined") {
			d.referrerPolicy = o;
		}
		return s(d, u).then((f) => ({
			image: f,
			extent: l,
			pixelRatio: c
		}));
	};
}
function dT(n, e, t) {
	if (n.url === "undefined") return;
	const i = H(n.projection || "EPSG:3857"), s = gs(e, t, 0, Bc), r = {
		QUERY_LAYERS: n.params.LAYERS,
		INFO_FORMAT: "application/json"
	};
	Object.assign(r, qr(n.params, "GetFeatureInfo"), n.params);
	const o = gn((e[0] - s[0]) / t, Ft), a = gn((s[3] - e[1]) / t, Ft), l = Ha(r.VERSION, "1.3") >= 0;
	r[l ? "I" : "X"] = o;
	r[l ? "J" : "Y"] = a;
	return qd(n.url, s, Bc, i, r);
}
function fT(n, e) {
	if (n.url === "undefined") return;
	const t = {
		SERVICE: "WMS",
		VERSION: Hr,
		REQUEST: "GetLegendGraphic",
		FORMAT: "image/png"
	};
	const i = H(n.projection || "EPSG:3857").getMetersPerUnit() || 1;
	const s = 28e-5;
	t.SCALE = e * i / 28e-5;
	Object.assign(t, n.params);
	if (n.params !== "undefined" && t.LAYER === "undefined") {
		const i = t.LAYERS;
		if (!(!Array.isArray(t.LAYERS) || t.LAYERS.length !== 1)) return;
		t.LAYER = t.LAYERS;
	}
	return An(n.url, t);
}
class gT extends rT {
	constructor(e) {
		e = e || {};
		super({
			attributions: e.attributions,
			interpolate: e.interpolate,
			projection: e.projection,
			resolutions: e.resolutions
		});
		this.crossOrigin_ = e.crossOrigin !== "undefined" ? e.crossOrigin : null;
		this.referrerPolicy_ = e.referrerPolicy;
		this.url_ = e.url;
		this.imageLoadFunction_ = e.imageLoadFunction !== "undefined" ? e.imageLoadFunction : hh;
		this.params_ = Object.assign({}, e.params);
		this.serverType_ = e.serverType;
		this.hidpi_ = e.hidpi !== "undefined" ? e.hidpi : true;
		this.renderedRevision_ = 0;
		this.ratio_ = e.ratio !== "undefined" ? e.ratio : 1.5;
		this.loaderProjection_ = null;
	}
	getFeatureInfoUrl(e, t, i, s) {
		const r = H(i), o = this.getProjection();
		if (o && o !== r) {
			t = ws(o, r, e, t);
			e = On(e, r, o);
		}
		const a = {
			url: this.url_,
			params: {
				...this.params_,
				...s
			},
			projection: o || r
		};
		return dT(a, e, t);
	}
	getLegendUrl(e, t) {
		return fT({
			url: this.url_,
			params: {
				...this.params_,
				...t
			}
		}, e);
	}
	getParams() {
		return this.params_;
	}
	getImageInternal(e, t, i, s) {
		return this.url_ === "undefined" ? null : ((!this.loader || this.loaderProjection_ !== s) && (this.loaderProjection_ = s, this.loader = uT({
			crossOrigin: this.crossOrigin_,
			referrerPolicy: this.referrerPolicy_,
			params: this.params_,
			projection: s,
			serverType: this.serverType_,
			hidpi: this.hidpi_,
			url: this.url_,
			ratio: this.ratio_,
			load: (r, o) => this.image.setImage(r)
		})), super.getImageInternal(e, t, i, s));
	}
	getImageLoadFunction() {
		return this.imageLoadFunction_;
	}
	getUrl() {
		return this.url_;
	}
	setImageLoadFunction(e) {
		this.imageLoadFunction_ = e;
		this.changed();
	}
	setUrl(e) {
		if (e != this.url_) {
			this.url_ = e;
			this.loader = null;
			this.changed();
		}
	}
	setParams(e) {
		this.params_ = Object.assign({}, e);
		this.loader = null;
		this.changed();
	}
	updateParams(e) {
		Object.assign(this.params_, e);
		this.changed();
	}
	changed() {
		this.image = null;
		super.changed();
	}
}
class _T extends oh {
	constructor(e) {
		const t = e.projection || "EPSG:3857", i = e.extent || xo(t), s = e.tileGrid || Ud({
			extent: i,
			maxResolution: e.maxResolution,
			maxZoom: e.maxZoom !== "undefined" ? e.maxZoom : 22,
			minZoom: e.minZoom,
			tileSize: e.tileSize || 512
		});
		super({
			attributions: e.attributions,
			attributionsCollapsible: e.attributionsCollapsible,
			cacheSize: e.cacheSize,
			interpolate: true,
			projection: t,
			state: e.state,
			tileGrid: s,
			tileLoadFunction: e.tileLoadFunction ? e.tileLoadFunction : mT,
			tileUrlFunction: e.tileUrlFunction,
			url: e.url,
			urls: e.urls,
			wrapX: e.wrapX === "undefined" ? true : e.wrapX,
			transition: e.transition,
			zDirection: e.zDirection === "undefined" ? 1 : e.zDirection
		});
		this.format_ = e.format ? e.format : null;
		this.tileKeysBySourceTileUrl_ = {};
		this.sourceTiles_ = {};
		this.overlaps_ = e.overlaps == null ? true : e.overlaps;
		this.tileClass = e.tileClass ? e.tileClass : $y;
		this.tileGrids_ = {};
	}
	getOverlaps() {
		return this.overlaps_;
	}
	getSourceTiles(e, t, i) {
		if (i.getState() === M.IDLE) {
			i.setState(M.LOADING);
			const s = i.wrappedTileCoord;
			const r = this.getTileGridForProjection(t);
			let o = r.getTileCoordExtent(i.wrappedTileCoord);
			const a = i.wrappedTileCoord[0];
			const l = r.getResolution(i.wrappedTileCoord[0]);
			tt(o, -l, o);
			const h = this.projection;
			if (t && this.projection && !Ae(t, this.projection)) {
				o = as(o, t, this.projection);
			}
			const c = this.tileGrid;
			const u = this.tileGrid.getExtent();
			if (u) {
				Et(o, u, o);
			}
			let d = l;
			if (t && this.projection && !Ae(t, this.projection)) {
				d = l / this.projection.getMetersPerUnit() / t.getMetersPerUnit();
			}
			const f = this.tileGrid.getZForResolution(d, this.zDirection);
			this.tileGrid.forEachTileCoord(o, f, (g) => {
				const m = this.tileUrlFunction(g, e, t);
				if (!this.sourceTiles_[m]) {
					this.sourceTiles_[m] = new this.tileClass(g, m ? M.IDLE : M.EMPTY, m, this.format_, this.tileLoadFunction);
				}
				const _ = this.sourceTiles_[m];
				i.sourceTiles.push(this.sourceTiles_[m]);
				if (!this.tileKeysBySourceTileUrl_[m]) {
					this.tileKeysBySourceTileUrl_[m] = [];
				}
				this.tileKeysBySourceTileUrl_[m].push(i.getKey());
				const p = this.sourceTiles_[m].getState();
				if (p < M.LOADED) {
					const y = (E) => {
						this.handleTileChange(E);
						const x = _.getState();
						if (x === M.LOADED || x === M.ERROR) {
							const T = _.getKey();
							T in i.errorTileKeys ? _.getState() === M.LOADED && delete i.errorTileKeys[T] : i.loadingSourceTiles--;
							x === M.ERROR ? i.errorTileKeys[T] = true : _.removeEventListener(U.CHANGE, y);
							if (i.loadingSourceTiles === 0) {
								i.setState(si(i.errorTileKeys) ? M.LOADED : M.ERROR);
							}
						}
					};
					_.addEventListener(U.CHANGE, y);
					i.loadingSourceTiles++;
				}
				if (p === M.IDLE) {
					this.sourceTiles_[m].extent = c.getTileCoordExtent(g);
					this.sourceTiles_[m].projection = this.projection;
					this.sourceTiles_[m].resolution = c.getResolution(g[0]);
					this.sourceTiles_[m].load();
				}
			});
			if (!i.loadingSourceTiles) {
				i.setState(i.sourceTiles.some((g) => g.getState() === M.ERROR) ? M.ERROR : M.LOADED);
			}
		}
		return i.sourceTiles;
	}
	removeSourceTiles(e) {
		const t = e.getKey(), i = e.sourceTiles;
		for (let s = 0, r = e.sourceTiles.length; s < r; ++s) {
			const o = i[s].getTileUrl();
			if (!this.tileKeysBySourceTileUrl_[o]) return;
			const a = this.tileKeysBySourceTileUrl_[o].indexOf(t);
			if (a !== -1) {
				this.tileKeysBySourceTileUrl_[o].splice(a, 1);
				if (this.tileKeysBySourceTileUrl_[o].length === 0) {
					delete this.tileKeysBySourceTileUrl_[o];
					delete this.sourceTiles_[o];
				}
			}
		}
	}
	getTile(e, t, i, s, r) {
		const o = [
			e,
			t,
			i
		];
		let a = this.getTileCoordForTileUrlFunction(o, r);
		const l = this.getTileGrid().getExtent(), h = this.projection, c = this.getTileGridForProjection(r);
		if (a && l) {
			const f = c.getTileCoordExtent(a);
			tt(f, -c.getResolution(e), f);
			if (!me(l, !r || !h || Ae(r, h) ? f : as(f, r, h))) {
				a = null;
			}
		}
		let u = true;
		if (a !== null) {
			const f = this.tileGrid;
			const g = c.getResolution(e);
			let m = g;
			if (r && h && !Ae(r, h)) {
				m = g / h.getMetersPerUnit() / r.getMetersPerUnit();
			}
			const _ = this.tileGrid.getZForResolution(m, 1);
			const p = c.getTileCoordExtent(a);
			tt(p, -g, p);
			this.tileGrid.forEachTileCoord(!r || !h || Ae(r, h) ? p : as(p, r, h), _, (y) => {
				u = u && !this.tileUrlFunction(y, s, h);
			});
		}
		const d = new Gy(o, u ? M.EMPTY : M.IDLE, a, this.getSourceTiles.bind(this, s, r), this.removeSourceTiles.bind(this));
		d.key = this.getKey();
		return d;
	}
	getTileGridForProjection(e) {
		const t = e.getCode();
		let i = this.tileGrids_[t];
		if (!i) {
			const s = this.projection;
			if (this.projection !== null && !Ae(this.projection, e)) return rh(e);
			const r = this.tileGrid;
			const o = this.tileGrid.getResolutions().slice();
			const a = o.map(function(c, u) {
				return r.getOrigin(u);
			});
			const l = o.map(function(c, u) {
				return r.getTileSize(u);
			});
			const h = cd + 1;
			for (let c = o.length; c < h; ++c) {
				o.push(o[c - 1] / 2);
				a.push(a[c - 1]);
				l.push(l[c - 1]);
			}
			i = new LE({
				extent: this.tileGrid.getExtent(),
				origins: a,
				resolutions: o,
				tileSizes: l
			});
			this.tileGrids_[t] = i;
		}
		return i;
	}
	getTilePixelRatio(e) {
		return e;
	}
	getTilePixelSize(e, t, i) {
		const s = this.getTileGridForProjection(i), r = Ne(s.getTileSize(e), this.tmpSize);
		return [Math.round(r[0] * t), Math.round(r[1] * t)];
	}
	setOverlaps(e) {
		this.overlaps_ = e;
		this.changed();
	}
}
function mT(n, e) {
	n.setLoader(function(t, i, s) {
		tu(e, n.getFormat(), t, i, s, n.onLoad.bind(n), n.onError.bind(n));
	});
}
class pT extends Im {
	constructor(e) {
		super(e);
		this.image = null;
		this.renderedSourceRevision_ = 0;
	}
	getImage() {
		return this.image ? this.image.getImage() : null;
	}
	prepareFrame(e) {
		const t = e.layerStatesArray[e.layerIndex], i = e.pixelRatio, s = e.viewState, r = e.viewState.resolution, o = this.getLayer().getSource(), a = e.viewHints;
		let l = e.extent;
		if (e.layerStatesArray[e.layerIndex].extent !== "undefined") {
			l = Et(l, lt(e.layerStatesArray[e.layerIndex].extent, e.viewState.projection));
		}
		if (!e.viewHints[de.ANIMATING] && !e.viewHints[de.INTERACTING] && !Pi(l)) if (o) {
			if (!this.getLayer().rendered && this.renderedSourceRevision_ !== o.getRevision()) {
				this.image = null;
			}
			this.renderedSourceRevision_ = o.getRevision();
			const h = s.projection;
			const c = o.getImage(l, r, i, s.projection);
			if (c) {
				this.loadImage(c) ? this.image = c : c.getState() === $.EMPTY && (this.image = null);
			}
		} else this.image = null;
		return !!this.image;
	}
	getData(e) {
		const t = this.frameState;
		if (!this.frameState) return null;
		const i = this.getLayer(), s = xe(this.frameState.pixelToCoordinateTransform, e.slice()), r = i.getExtent();
		if (r && !Ui(r, s)) return null;
		const o = this.image.getExtent(), a = this.image.getImage(), l = J(o), h = Math.floor(a.width * ((s[0] - o[0]) / l));
		if (h < 0 || h >= a.width) return null;
		const c = Ce(o), u = Math.floor(a.height * ((o[3] - s[1]) / c));
		return u < 0 || u >= a.height ? null : this.getImageData(a, h, u);
	}
	renderFrame(e, t) {
		const i = this.image, s = this.image.getExtent(), r = this.image.getResolution(), [o, a] = Array.isArray(r) ? r : [r, r], l = this.image.getPixelRatio(), h = e.layerStatesArray[e.layerIndex], c = e.pixelRatio, u = e.viewState, d = e.viewState.center, f = e.viewState.resolution, g = e.pixelRatio * o / (e.viewState.resolution * l), m = e.pixelRatio * a / (e.viewState.resolution * l);
		this.prepareContainer(e, t);
		const _ = this.context.canvas.width, p = this.context.canvas.height, y = this.getRenderContext(e);
		let E = false, x = true;
		if (e.layerStatesArray[e.layerIndex].extent) {
			const R = lt(h.extent, u.projection);
			x = me(R, e.extent);
			E = x && !at(R, e.extent);
			if (E) {
				this.clipUnrotated(y, e, R);
			}
		}
		const T = this.image.getImage(), v = gt(this.tempTransform, this.context.canvas.width / 2, this.context.canvas.height / 2, g, m, 0, l * (s[0] - e.viewState.center[0]) / o, l * (e.viewState.center[1] - s[3]) / a);
		this.renderedResolution = a * e.pixelRatio / l;
		const P = T.width * v[0], S = T.height * v[3];
		if (!this.getLayer().getSource().getInterpolate()) {
			y.imageSmoothingEnabled = false;
		}
		this.preRender(y, e);
		if (x && P >= .5 && S >= .5) {
			const R = v[4];
			const I = v[5];
			const N = h.opacity;
			if (h.opacity !== 1) {
				y.save();
				y.globalAlpha = h.opacity;
			}
			y.drawImage(T, 0, 0, +T.width, +T.height, v[4], v[5], P, S);
			if (h.opacity !== 1) {
				y.restore();
			}
		}
		this.postRender(this.context, e);
		y.imageSmoothingEnabled = true;
		return this.container;
	}
}
class xT extends wp {
	constructor(e) {
		e = e || {};
		super(e);
	}
}
class TT extends xT {
	constructor(e) {
		super(e);
	}
	createRenderer() {
		return new pT(this);
	}
	getData(e) {
		return super.getData(e);
	}
}
class CT extends WE {
	constructor(e) {
		e = e || {};
		const t = Object.assign({}, e.params);
		super({
			attributions: e.attributions,
			attributionsCollapsible: e.attributionsCollapsible,
			cacheSize: e.cacheSize,
			crossOrigin: e.crossOrigin,
			interpolate: e.interpolate,
			projection: e.projection,
			reprojectionErrorThreshold: e.reprojectionErrorThreshold,
			tileClass: e.tileClass,
			tileGrid: e.tileGrid,
			tileLoadFunction: e.tileLoadFunction,
			url: e.url,
			urls: e.urls,
			wrapX: e.wrapX !== "undefined" ? e.wrapX : true,
			transition: e.transition,
			zDirection: e.zDirection
		});
		this.gutter_ = e.gutter !== "undefined" ? e.gutter : 0;
		this.params_ = t;
		this.v13_ = true;
		this.serverType_ = e.serverType;
		this.hidpi_ = e.hidpi !== "undefined" ? e.hidpi : true;
		this.tmpExtent_ = je();
		this.updateV13_();
		this.setKey(this.getKeyForParams_());
	}
	getFeatureInfoUrl(e, t, i, s) {
		const r = H(i), o = this.getProjection() || r;
		let a = this.getTileGrid();
		if (!a) {
			a = this.getTileGridForProjection(o);
		}
		const l = On(e, r, o), h = ws(o, r, e, t), c = a.getZForResolution(h, this.zDirection), u = a.getResolution(c), d = a.getTileCoordForCoordAndZ(l, c);
		if (a.getResolutions().length <= d[0]) return;
		let f = a.getTileCoordExtent(d, this.tmpExtent_);
		const g = this.gutter_;
		if (this.gutter_ !== 0) {
			f = tt(f, u * this.gutter_, f);
		}
		const m = { QUERY_LAYERS: this.params_.LAYERS };
		Object.assign(m, qr(this.params_, "GetFeatureInfo"), s);
		const _ = Math.floor((l[0] - f[0]) / u), p = Math.floor((f[3] - l[1]) / u);
		m[this.v13_ ? "I" : "X"] = _;
		m[this.v13_ ? "J" : "Y"] = p;
		return this.getRequestUrl_(d, f, 1, o || r, m);
	}
	getLegendUrl(e, t) {
		if (this.urls[0] === "undefined") return;
		const i = {
			SERVICE: "WMS",
			VERSION: Hr,
			REQUEST: "GetLegendGraphic",
			FORMAT: "image/png"
		};
		if (t === "undefined" || t.LAYER === "undefined") {
			const s = this.params_.LAYERS;
			if (!(!Array.isArray(this.params_.LAYERS) || this.params_.LAYERS.length === 1)) return;
			i.LAYER = this.params_.LAYERS;
		}
		const s = this.getProjection() ? this.getProjection().getMetersPerUnit() : 1;
		const r = 28e-5;
		i.SCALE = e * s / 28e-5;
		Object.assign(i, t);
		return An(this.urls[0], i);
	}
	getGutter() {
		return this.gutter_;
	}
	getParams() {
		return this.params_;
	}
	getRequestUrl_(e, t, i, s, r) {
		const o = this.urls;
		if (!this.urls) return;
		let a;
		if (this.urls.length == 1) a = this.urls[0];
		else {
			const l = Qt(Gd(e), o.length);
			a = o[l];
		}
		return Jd(t, (this.tileGrid || this.getTileGridForProjection(s)).getResolution(e[0]), i, s, a, r, this.serverType_);
	}
	getTilePixelRatio(e) {
		return !this.hidpi_ || this.serverType_ === "undefined" ? 1 : e;
	}
	getKeyForParams_() {
		let e = 0;
		const t = [];
		for (const i in this.params_) t[e++] = i + "-" + this.params_[i];
		return t.join("/");
	}
	setParams_(e) {
		this.params_ = e;
		this.updateV13_();
		this.setKey(this.getKeyForParams_());
	}
	setParams(e) {
		this.setParams_(Object.assign({}, e));
	}
	updateParams(e) {
		this.setParams_(Object.assign(this.params_, e));
	}
	updateV13_() {
		const e = this.params_.VERSION || Hr;
		this.v13_ = Ha(e, "1.3") >= 0;
	}
	tileUrlFunction(e, t, i) {
		let s = this.getTileGrid();
		if (!s) {
			s = this.getTileGridForProjection(i);
		}
		if (s.getResolutions().length <= e[0]) return;
		if (t != 1 && (!this.hidpi_ || this.serverType_ === "undefined")) {
			t = 1;
		}
		const r = s.getResolution(e[0]);
		let o = s.getTileCoordExtent(e, this.tmpExtent_);
		const a = this.gutter_;
		if (this.gutter_ !== 0) {
			o = tt(o, r * this.gutter_, o);
		}
		const l = Object.assign({}, qr(this.params_, "GetMap"));
		return this.getRequestUrl_(e, o, t, i, l);
	}
}
class RT extends WE {
	constructor(e) {
		const t = e.requestEncoding !== "undefined" ? e.requestEncoding : "KVP", i = e.tileGrid;
		let s = e.urls;
		if (s === "undefined" && e.url !== "undefined") {
			s = zd(e.url);
		}
		super({
			attributions: e.attributions,
			attributionsCollapsible: e.attributionsCollapsible,
			cacheSize: e.cacheSize,
			crossOrigin: e.crossOrigin,
			referrerPolicy: e.referrerPolicy,
			interpolate: e.interpolate,
			projection: e.projection,
			reprojectionErrorThreshold: e.reprojectionErrorThreshold,
			tileClass: e.tileClass,
			tileGrid: e.tileGrid,
			tileLoadFunction: e.tileLoadFunction,
			tilePixelRatio: e.tilePixelRatio,
			urls: s,
			wrapX: e.wrapX !== "undefined" ? e.wrapX : false,
			transition: e.transition,
			zDirection: e.zDirection
		});
		this.version_ = e.version !== "undefined" ? e.version : "1.0.0";
		this.format_ = e.format !== "undefined" ? e.format : "image/jpeg";
		this.dimensions_ = e.dimensions !== "undefined" ? e.dimensions : {};
		this.layer_ = e.layer;
		this.matrixSet_ = e.matrixSet;
		this.style_ = e.style;
		this.requestEncoding_ = t;
		this.setKey(this.getKeyForDimensions_());
		if (s && s.length > 0) {
			this.tileUrlFunction = ja(s.map(this.createFromWMTSTemplate.bind(this)));
		}
	}
	setUrls(e) {
		this.urls = e;
		const t = e.join("\n");
		this.setTileUrlFunction(ja(e.map(this.createFromWMTSTemplate.bind(this))), t);
	}
	getDimensions() {
		return this.dimensions_;
	}
	getFormat() {
		return this.format_;
	}
	getLayer() {
		return this.layer_;
	}
	getMatrixSet() {
		return this.matrixSet_;
	}
	getRequestEncoding() {
		return this.requestEncoding_;
	}
	getStyle() {
		return this.style_;
	}
	getVersion() {
		return this.version_;
	}
	getKeyForDimensions_() {
		const e = this.urls ? this.urls.slice(0) : [];
		for (const t in this.dimensions_) e.push(t + "-" + this.dimensions_[t]);
		return e.join("/");
	}
	updateDimensions(e) {
		Object.assign(this.dimensions_, e);
		this.setKey(this.getKeyForDimensions_());
	}
	createFromWMTSTemplate(e) {
		const t = this.requestEncoding_, i = {
			layer: this.layer_,
			style: this.style_,
			tilematrixset: this.matrixSet_
		};
		if (this.requestEncoding_ == "KVP") {
			Object.assign(i, {
				Service: "WMTS",
				Request: "GetTile",
				Version: this.version_,
				Format: this.format_
			});
		}
		e = this.requestEncoding_ == "KVP" ? An(e, i) : e.replace(/\{(\w+?)\}/g, function(o, a) {
			return a.toLowerCase() in i ? i[a.toLowerCase()] : o;
		});
		const s = this.tileGrid, r = this.dimensions_;
		return function(o, a, l) {
			if (!o) return;
			const h = {
				TileMatrix: s.getMatrixId(o[0]),
				TileCol: o[1],
				TileRow: o[2]
			};
			Object.assign(h, r);
			let c = e;
			t == "KVP" ? c = An(c, h) : c = c.replace(/\{(\w+?)\}/g, function(u, d) {
				return encodeURIComponent(h[d]);
			});
			return c;
		};
	}
}
function dR(n, e) {
	var A, W;
	const t = n.Contents.Layer, i = n.Contents.Layer == null ? "undefined" : n.Contents.Layer.find(function(w) {
		return w.Identifier == e.layer;
	});
	if (!i) return null;
	const s = n.Contents.TileMatrixSet;
	let r;
	i.TileMatrixSetLink.length > 1 ? "projection" in e ? r = i.TileMatrixSetLink.findIndex(function(w) {
		const D = s.find(function(q) {
			return q.Identifier == w.TileMatrixSet;
		}).SupportedCRS, k = H(s.find(function(q) {
			return q.Identifier == w.TileMatrixSet;
		}).SupportedCRS), B = H(e.projection);
		return k && B ? Ae(k, B) : s.find(function(q) {
			return q.Identifier == w.TileMatrixSet;
		}).SupportedCRS == e.projection;
	}) : r = i.TileMatrixSetLink.findIndex(function(w) {
		return w.TileMatrixSet == e.matrixSet;
	}) : r = 0;
	if (r < 0) {
		r = 0;
	}
	const o = i.TileMatrixSetLink[r].TileMatrixSet, a = i.TileMatrixSetLink[r].TileMatrixSetLimits;
	let l = i.Format[0];
	if ("format" in e) {
		l = e.format;
	}
	r = i.Style.findIndex(function(w) {
		return "style" in e ? w.Title == e.style : w.isDefault;
	});
	if (r < 0) {
		r = 0;
	}
	const h = i.Style[r].Identifier, c = {};
	if ("Dimension" in i) {
		i.Dimension.forEach(function(w, b, D) {
			const k = w.Identifier;
			let B = w.Default;
			if (B === "undefined") {
				B = w.Value[0];
			}
			c[w.Identifier] = B;
		});
	}
	const d = n.Contents.TileMatrixSet.find(function(w) {
		return w.Identifier == o;
	});
	let f;
	const g = d.SupportedCRS;
	if (d.SupportedCRS) {
		f = H(d.SupportedCRS);
	}
	if ("projection" in e) {
		const w = H(e.projection);
		if (w && (!f || Ae(w, f))) {
			f = w;
		}
	}
	let m = false;
	const _ = f.getAxisOrientation().startsWith("ne");
	let p = d.TileMatrix[0], y = {
		MinTileCol: 0,
		MinTileRow: 0,
		MaxTileCol: p.MatrixWidth - 1,
		MaxTileRow: p.MatrixHeight - 1
	};
	if (i.TileMatrixSetLink[r].TileMatrixSetLimits) {
		y = a[a.length - 1];
		const w = d.TileMatrix.find((b) => );
		if (w) {
			p = w;
		}
	}
	const E = (A = i.BoundingBox) == null ? "undefined" : A.find((w) => ), x = p.ScaleDenominator * 28e-5 / f.getMetersPerUnit(), T = _ ? [p.TopLeftCorner[1], p.TopLeftCorner[0]] : p.TopLeftCorner, v = p.TileWidth * x, P = p.TileHeight * x;
	let S = (W = E == null ? "undefined" : E.extent) != null ? W : d.BoundingBox;
	if (S && _) {
		S = [
			S[1],
			S[0],
			S[3],
			S[2]
		];
	}
	let R = [
		T[0] + v * y.MinTileCol,
		T[1] - P * (1 + y.MaxTileRow),
		T[0] + v * (1 + y.MaxTileCol),
		T[1] - P * y.MinTileRow
	];
	if (S !== "undefined" && !at(S, R)) {
		const w = i.WGS84BoundingBox;
		const b = H("EPSG:4326").getExtent();
		R = S;
		if (i.WGS84BoundingBox) m = i.WGS84BoundingBox[0] === b[0] && i.WGS84BoundingBox[2] === b[2];
		else {
			const D = as(S, d.SupportedCRS, "EPSG:4326");
			m = D[0] - 1e-10 <= b[0] && D[2] + 1e-10 >= b[2];
		}
	}
	const I = AE(d, R, i.TileMatrixSetLink[r].TileMatrixSetLimits), N = [];
	let L = e.requestEncoding;
	L = L !== "undefined" ? L : "";
	if ("OperationsMetadata" in n && "GetTile" in n.OperationsMetadata) {
		const w = n.OperationsMetadata.GetTile.DCP.HTTP.Get;
		for (let b = 0, D = n.OperationsMetadata.GetTile.DCP.HTTP.Get.length; b < D; ++b) if (n.OperationsMetadata.GetTile.DCP.HTTP.Get[b].Constraint) {
			const B = w[b].Constraint.find(function(q) {
				return q.name == "GetEncoding";
			}).AllowedValues.Value;
			if (L === "") {
				L = w[b].Constraint.find(function(q) {
					return q.name == "GetEncoding";
				}).AllowedValues.Value[0];
			}
			if (L === "KVP") w[b].Constraint.find(function(q) {
				return q.name == "GetEncoding";
			}).AllowedValues.Value.includes("KVP") && N.push(w[b].href);
			else break;
		} else n.OperationsMetadata.GetTile.DCP.HTTP.Get[b].href && (L = "KVP", N.push(n.OperationsMetadata.GetTile.DCP.HTTP.Get[b].href));
	}
	if (N.length === 0) {
		L = "REST";
		i.ResourceURL.forEach(function(w) {
			if (w.resourceType === "tile") {
				l = w.format;
				N.push(w.template);
			}
		});
	}
	return {
		urls: N,
		layer: e.layer,
		matrixSet: i.TileMatrixSetLink[r].TileMatrixSet,
		format: l,
		projection: f,
		requestEncoding: L,
		tileGrid: I,
		style: i.Style[r].Identifier,
		dimensions: c,
		wrapX: m,
		crossOrigin: e.crossOrigin
	};
}
const Jr = 34962;
const dh = 34963;
const Sr = 35048;
const Qd = 5126;
const $c = [
	"experimental-webgl",
	"webgl",
	"webkit-3d",
	"moz-webgl"
];
function FT(n, e) {
	e = Object.assign({
		preserveDrawingBuffer: true,
		antialias: !c_
	}, e);
	const t = $c.length;
	for (let i = 0; i < $c.length; ++i) try {
		const s = n.getContext($c[i], e);
		if (s) return s;
	} catch (s) {}
	return null;
}
const LT = {
	STATIC_DRAW: 35044,
	STREAM_DRAW: 35040,
	DYNAMIC_DRAW: 35048
};
class AT {
	constructor(e, t) {
		this.array_ = null;
		this.type_ = e;
		ee(e === Jr || e === dh, "A `WebGLArrayBuffer` must either be of type `ELEMENT_ARRAY_BUFFER` or `ARRAY_BUFFER`");
		this.usage_ = t !== "undefined" ? t : LT.STATIC_DRAW;
	}
	ofSize(e) {
		this.array_ = new (yr(this.type_))(e);
		return this;
	}
	fromArray(e) {
		this.array_ = yr(this.type_).from(e);
		return this;
	}
	fromArrayBuffer(e) {
		this.array_ = new (yr(this.type_))(e);
		return this;
	}
	getType() {
		return this.type_;
	}
	getArray() {
		return this.array_;
	}
	setArray(e) {
		const t = yr(this.type_);
		if (!(e instanceof t)) throw new Error("Expected ".concat(t));
		this.array_ = e;
	}
	getUsage() {
		return this.usage_;
	}
	getSize() {
		return this.array_ ? this.array_.length : 0;
	}
}
function yr(n) {
	switch (n) {
		case Jr: return Float32Array;
		case dh: return Uint32Array;
		default: return Float32Array;
	}
}
const xr = {
	LOST: "webglcontextlost",
	RESTORED: "webglcontextrestored"
};
const MT = "\n  precision mediump float;\n\n  attribute vec2 a_position;\n  varying vec2 v_texCoord;\n  varying vec2 v_screenCoord;\n\n  uniform vec2 u_screenSize;\n\n  void main() {\n    v_texCoord = a_position * 0.5 + 0.5;\n    v_screenCoord = v_texCoord * u_screenSize;\n    gl_Position = vec4(a_position, 0.0, 1.0);\n  }\n";
const bT = "\n  precision mediump float;\n\n  uniform sampler2D u_image;\n  uniform float u_opacity;\n\n  varying vec2 v_texCoord;\n\n  void main() {\n    gl_FragColor = texture2D(u_image, v_texCoord) * u_opacity;\n  }\n";
class OT {
	constructor(e) {
		this.gl_ = e.webGlContext;
		const t = this.gl_;
		this.scaleRatio_ = e.scaleRatio || 1;
		this.renderTargetTexture_ = this.gl_.createTexture();
		this.renderTargetTextureSize_ = null;
		this.frameBuffer_ = this.gl_.createFramebuffer();
		this.depthBuffer_ = this.gl_.createRenderbuffer();
		const i = this.gl_.createShader(this.gl_.VERTEX_SHADER);
		this.gl_.shaderSource(i, e.vertexShader || MT);
		this.gl_.compileShader(i);
		const s = this.gl_.createShader(this.gl_.FRAGMENT_SHADER);
		this.gl_.shaderSource(s, e.fragmentShader || bT);
		this.gl_.compileShader(s);
		this.renderTargetProgram_ = this.gl_.createProgram();
		this.gl_.attachShader(this.renderTargetProgram_, i);
		this.gl_.attachShader(this.renderTargetProgram_, s);
		this.gl_.linkProgram(this.renderTargetProgram_);
		this.renderTargetVerticesBuffer_ = this.gl_.createBuffer();
		const r = [
			-1,
			-1,
			1,
			-1,
			-1,
			1,
			1,
			-1,
			1,
			1,
			-1,
			1
		];
		this.gl_.bindBuffer(this.gl_.ARRAY_BUFFER, this.renderTargetVerticesBuffer_);
		this.gl_.bufferData(this.gl_.ARRAY_BUFFER, new Float32Array(r), this.gl_.STATIC_DRAW);
		this.renderTargetAttribLocation_ = this.gl_.getAttribLocation(this.renderTargetProgram_, "a_position");
		this.renderTargetUniformLocation_ = this.gl_.getUniformLocation(this.renderTargetProgram_, "u_screenSize");
		this.renderTargetOpacityLocation_ = this.gl_.getUniformLocation(this.renderTargetProgram_, "u_opacity");
		this.renderTargetTextureLocation_ = this.gl_.getUniformLocation(this.renderTargetProgram_, "u_image");
		this.uniforms_ = [];
		if (e.uniforms) {
			Object.keys(e.uniforms).forEach((o) => {
				this.uniforms_.push({
					value: e.uniforms[o],
					location: t.getUniformLocation(this.renderTargetProgram_, o)
				});
			});
		}
	}
	getRenderTargetTexture() {
		return this.renderTargetTexture_;
	}
	getGL() {
		return this.gl_;
	}
	init(e) {
		const t = this.getGL(), i = [t.drawingBufferWidth * this.scaleRatio_, t.drawingBufferHeight * this.scaleRatio_];
		t.bindFramebuffer(t.FRAMEBUFFER, this.getFrameBuffer());
		t.bindRenderbuffer(t.RENDERBUFFER, this.getDepthBuffer());
		t.viewport(0, 0, i[0], i[1]);
		if (!this.renderTargetTextureSize_ || this.renderTargetTextureSize_[0] !== i[0] || this.renderTargetTextureSize_[1] !== i[1]) {
			this.renderTargetTextureSize_ = i;
			const s = 0;
			const r = t.RGBA;
			const o = 0;
			const a = t.RGBA;
			const l = t.UNSIGNED_BYTE;
			const h = null;
			t.bindTexture(t.TEXTURE_2D, this.renderTargetTexture_);
			t.texImage2D(t.TEXTURE_2D, 0, t.RGBA, i[0], i[1], 0, t.RGBA, t.UNSIGNED_BYTE, null);
			t.texParameteri(t.TEXTURE_2D, t.TEXTURE_MIN_FILTER, t.LINEAR);
			t.texParameteri(t.TEXTURE_2D, t.TEXTURE_WRAP_S, t.CLAMP_TO_EDGE);
			t.texParameteri(t.TEXTURE_2D, t.TEXTURE_WRAP_T, t.CLAMP_TO_EDGE);
			t.framebufferTexture2D(t.FRAMEBUFFER, t.COLOR_ATTACHMENT0, t.TEXTURE_2D, this.renderTargetTexture_, 0);
			t.renderbufferStorage(t.RENDERBUFFER, t.DEPTH_COMPONENT16, i[0], i[1]);
			t.framebufferRenderbuffer(t.FRAMEBUFFER, t.DEPTH_ATTACHMENT, t.RENDERBUFFER, this.depthBuffer_);
		}
	}
	apply(e, t, i, s) {
		const r = this.getGL(), o = e.size;
		r.bindFramebuffer(r.FRAMEBUFFER, t ? t.getFrameBuffer() : null);
		r.activeTexture(r.TEXTURE0);
		r.bindTexture(r.TEXTURE_2D, this.renderTargetTexture_);
		if (!t) {
			const l = O(r.canvas);
			if (!e.renderTargets[l]) {
				const h = r.getContextAttributes();
				if (h && h.preserveDrawingBuffer) {
					r.clearColor(0, 0, 0, 0);
					r.clearDepth(1);
					r.clear(r.COLOR_BUFFER_BIT | r.DEPTH_BUFFER_BIT);
				}
				e.renderTargets[l] = true;
			}
		}
		r.disable(r.DEPTH_TEST);
		r.enable(r.BLEND);
		r.blendFunc(r.ONE, r.ONE_MINUS_SRC_ALPHA);
		r.viewport(0, 0, r.drawingBufferWidth, r.drawingBufferHeight);
		r.bindBuffer(r.ARRAY_BUFFER, this.renderTargetVerticesBuffer_);
		r.useProgram(this.renderTargetProgram_);
		r.enableVertexAttribArray(this.renderTargetAttribLocation_);
		r.vertexAttribPointer(this.renderTargetAttribLocation_, 2, r.FLOAT, false, 0, 0);
		r.uniform2f(this.renderTargetUniformLocation_, e.size[0], e.size[1]);
		r.uniform1i(this.renderTargetTextureLocation_, 0);
		const a = e.layerStatesArray[e.layerIndex].opacity;
		r.uniform1f(this.renderTargetOpacityLocation_, e.layerStatesArray[e.layerIndex].opacity);
		this.applyUniforms(e);
		if (i) {
			i(r, e);
		}
		r.drawArrays(r.TRIANGLES, 0, 6);
		if (s) {
			s(r, e);
		}
	}
	getFrameBuffer() {
		return this.frameBuffer_;
	}
	getDepthBuffer() {
		return this.depthBuffer_;
	}
	applyUniforms(e) {
		const t = this.getGL();
		let i, s = 1;
		this.uniforms_.forEach(function(r) {
			i = typeof r.value == "function" ? r.value(e) : r.value;
			if (i instanceof HTMLCanvasElement || i instanceof ImageData) {
				if (!r.texture) {
					r.texture = t.createTexture();
				}
				t.activeTexture(t["TEXTURE".concat(s)]);
				t.bindTexture(t.TEXTURE_2D, r.texture);
				t.texParameteri(t.TEXTURE_2D, t.TEXTURE_MIN_FILTER, t.LINEAR);
				t.texParameteri(t.TEXTURE_2D, t.TEXTURE_WRAP_S, t.CLAMP_TO_EDGE);
				t.texParameteri(t.TEXTURE_2D, t.TEXTURE_WRAP_T, t.CLAMP_TO_EDGE);
				i instanceof ImageData ? t.texImage2D(t.TEXTURE_2D, 0, t.RGBA, t.RGBA, i.width, i.height, 0, t.UNSIGNED_BYTE, new Uint8Array(i.data)) : t.texImage2D(t.TEXTURE_2D, 0, t.RGBA, t.RGBA, t.UNSIGNED_BYTE, i);
				t.uniform1i(r.location, s++);
			} else if (Array.isArray(i)) switch (i.length) {
				case 2:
					t.uniform2f(r.location, i[0], i[1]);
					return;
				case 3:
					t.uniform3f(r.location, i[0], i[1], i[2]);
					return;
				case 4:
					t.uniform4f(r.location, i[0], i[1], i[2], i[3]);
					return;
				default: return;
			}
			else typeof i == "number" && t.uniform1f(r.location, i);
		});
	}
}
const Vt = {
	PROJECTION_MATRIX: "u_projectionMatrix",
	SCREEN_TO_WORLD_MATRIX: "u_screenToWorldMatrix",
	TIME: "u_time",
	ZOOM: "u_zoom",
	RESOLUTION: "u_resolution",
	ROTATION: "u_rotation",
	VIEWPORT_SIZE_PX: "u_viewportSizePx",
	PIXEL_RATIO: "u_pixelRatio",
	HIT_DETECTION: "u_hitDetection"
};
const Oe = {
	UNSIGNED_BYTE: 5121,
	UNSIGNED_SHORT: 5123,
	UNSIGNED_INT: 5125,
	FLOAT: 5126
};
const Qr = {};
function jc(n) {
	return "shared/" + n;
}
let zc = 0;
function DT() {
	const n = "unique/" + zc;
	zc += 1;
	return n;
}
function NT(n) {
	let e = Qr[n];
	if (!e) {
		const t = document.createElement("canvas");
		t.width = 1;
		t.height = 1;
		t.style.position = "absolute";
		t.style.left = "0";
		e = {
			users: 0,
			context: FT(t)
		};
		Qr[n] = e;
	}
	e.users += 1;
	return e.context;
}
function kT(n) {
	const e = Qr[n];
	if (!Qr[n] || (Qr[n].users -= 1, Qr[n].users > 0)) return;
	const t = Qr[n].context, i = Qr[n].context.getExtension("WEBGL_lose_context");
	if (i) {
		i.loseContext();
	}
	const s = Qr[n].context.canvas;
	Qr[n].context.canvas.width = 1;
	Qr[n].context.canvas.height = 1;
	delete Qr[n];
}
class GT extends Tf {
	constructor(e) {
		super();
		e = e || {};
		this.boundHandleWebGLContextLost_ = this.handleWebGLContextLost.bind(this);
		this.boundHandleWebGLContextRestored_ = this.handleWebGLContextRestored.bind(this);
		this.canvasCacheKey_ = e.canvasCacheKey ? jc(e.canvasCacheKey) : DT();
		this.gl_ = NT(this.canvasCacheKey_);
		this.bufferCache_ = {};
		this.extensionCache_ = {};
		this.currentProgram_ = null;
		this.needsToBeRecreated_ = false;
		const t = this.gl_.canvas;
		this.gl_.canvas.addEventListener(xr.LOST, this.boundHandleWebGLContextLost_);
		this.gl_.canvas.addEventListener(xr.RESTORED, this.boundHandleWebGLContextRestored_);
		this.offsetRotateMatrix_ = Se();
		this.offsetScaleMatrix_ = Se();
		this.tmpMat4_ = Hd();
		this.uniformLocationsByProgram_ = {};
		this.attribLocationsByProgram_ = {};
		this.uniforms_ = [];
		if (e.uniforms) {
			this.setUniforms(e.uniforms);
		}
		this.postProcessPasses_ = e.postProcesses ? e.postProcesses.map((i) => new OT({
			webGlContext: this.gl_,
			scaleRatio: i.scaleRatio,
			vertexShader: i.vertexShader,
			fragmentShader: i.fragmentShader,
			uniforms: i.uniforms
		})) : [new OT({ webGlContext: this.gl_ })];
		this.shaderCompileErrors_ = null;
		this.startTime_ = Date.now();
		this.maxAttributeCount_ = this.gl_.getParameter(this.gl_.MAX_VERTEX_ATTRIBS);
	}
	setUniforms(e) {
		this.uniforms_ = [];
		this.addUniforms(e);
	}
	addUniforms(e) {
		for (const t in e) this.uniforms_.push({
			name: t,
			value: e[t]
		});
	}
	canvasCacheKeyMatches(e) {
		return this.canvasCacheKey_ === jc(e);
	}
	getExtension(e) {
		if (e in this.extensionCache_) return this.extensionCache_[e];
		const t = this.gl_.getExtension(e);
		this.extensionCache_[e] = t;
		return t;
	}
	getInstancedRenderingExtension_() {
		const e = this.getExtension("ANGLE_instanced_arrays");
		ee(!!e, "WebGL extension 'ANGLE_instanced_arrays' is required for vector rendering");
		return e;
	}
	bindBuffer(e) {
		const t = this.gl_, i = O(e);
		let s = this.bufferCache_[i];
		if (!s) {
			const r = t.createBuffer();
			s = {
				buffer: e,
				webGlBuffer: r
			};
			this.bufferCache_[i] = s;
		}
		this.gl_.bindBuffer(e.getType(), s.webGlBuffer);
	}
	flushBufferData(e) {
		const t = this.gl_;
		this.bindBuffer(e);
		this.gl_.bufferData(e.getType(), e.getArray(), e.getUsage());
	}
	deleteBuffer(e) {
		const t = O(e);
		delete this.bufferCache_[t];
	}
	disposeInternal() {
		const e = this.gl_.canvas;
		this.gl_.canvas.removeEventListener(xr.LOST, this.boundHandleWebGLContextLost_);
		this.gl_.canvas.removeEventListener(xr.RESTORED, this.boundHandleWebGLContextRestored_);
		kT(this.canvasCacheKey_);
		delete this.gl_;
	}
	prepareDraw(e, t, i) {
		const s = this.gl_, r = this.getCanvas(), o = e.size, a = e.pixelRatio;
		if (r.width !== e.size[0] * e.pixelRatio || r.height !== e.size[1] * e.pixelRatio) {
			r.width = e.size[0] * e.pixelRatio;
			r.height = e.size[1] * e.pixelRatio;
			r.style.width = e.size[0] + "px";
			r.style.height = e.size[1] + "px";
		}
		for (let l = this.postProcessPasses_.length - 1; l >= 0; l--) this.postProcessPasses_[l].init(e);
		this.gl_.bindTexture(this.gl_.TEXTURE_2D, null);
		this.gl_.clearColor(0, 0, 0, 0);
		this.gl_.depthRange(0, 1);
		this.gl_.clearDepth(1);
		this.gl_.clear(this.gl_.COLOR_BUFFER_BIT | this.gl_.DEPTH_BUFFER_BIT);
		this.gl_.enable(this.gl_.BLEND);
		this.gl_.blendFunc(this.gl_.ONE, t ? this.gl_.ZERO : this.gl_.ONE_MINUS_SRC_ALPHA);
		i ? (this.gl_.enable(this.gl_.DEPTH_TEST), this.gl_.depthFunc(this.gl_.LEQUAL)) : this.gl_.disable(this.gl_.DEPTH_TEST);
	}
	bindFrameBuffer(e, t) {
		const i = this.getGL();
		i.bindFramebuffer(i.FRAMEBUFFER, e);
		if (t) {
			i.framebufferTexture2D(i.FRAMEBUFFER, i.COLOR_ATTACHMENT0, i.TEXTURE_2D, t, 0);
		}
	}
	bindInitialFrameBuffer() {
		const e = this.getGL(), t = this.postProcessPasses_[0].getFrameBuffer();
		e.bindFramebuffer(e.FRAMEBUFFER, t);
		const i = this.postProcessPasses_[0].getRenderTargetTexture();
		e.framebufferTexture2D(e.FRAMEBUFFER, e.COLOR_ATTACHMENT0, e.TEXTURE_2D, i, 0);
	}
	bindTexture(e, t, i) {
		const s = this.gl_;
		this.gl_.activeTexture(this.gl_.TEXTURE0 + t);
		this.gl_.bindTexture(this.gl_.TEXTURE_2D, e);
		this.gl_.uniform1i(this.getUniformLocation(i), t);
	}
	bindAttribute(e, t, i) {
		const s = this.getGL();
		this.bindBuffer(e);
		const r = this.getAttributeLocation(t);
		s.enableVertexAttribArray(r);
		s.vertexAttribPointer(r, i, s.FLOAT, false, 0, 0);
	}
	prepareDrawToRenderTarget(e, t, i, s) {
		const r = this.gl_, o = t.getSize();
		this.gl_.bindFramebuffer(this.gl_.FRAMEBUFFER, t.getFramebuffer());
		this.gl_.bindRenderbuffer(this.gl_.RENDERBUFFER, t.getDepthbuffer());
		this.gl_.viewport(0, 0, o[0], o[1]);
		this.gl_.bindTexture(this.gl_.TEXTURE_2D, t.getTexture());
		this.gl_.clearColor(0, 0, 0, 0);
		this.gl_.depthRange(0, 1);
		this.gl_.clearDepth(1);
		this.gl_.clear(this.gl_.COLOR_BUFFER_BIT | this.gl_.DEPTH_BUFFER_BIT);
		this.gl_.enable(this.gl_.BLEND);
		this.gl_.blendFunc(this.gl_.ONE, i ? this.gl_.ZERO : this.gl_.ONE_MINUS_SRC_ALPHA);
		s ? (this.gl_.enable(this.gl_.DEPTH_TEST), this.gl_.depthFunc(this.gl_.LEQUAL)) : this.gl_.disable(this.gl_.DEPTH_TEST);
	}
	drawElements(e, t) {
		const i = this.gl_;
		this.getExtension("OES_element_index_uint");
		const s = this.gl_.UNSIGNED_INT, r = 4, o = t - e, a = e * 4;
		this.gl_.drawElements(this.gl_.TRIANGLES, o, this.gl_.UNSIGNED_INT, a);
	}
	drawElementsInstanced(e, t, i) {
		const s = this.gl_;
		this.getExtension("OES_element_index_uint");
		const r = this.getInstancedRenderingExtension_(), o = this.gl_.UNSIGNED_INT, a = 4, l = t - e, h = e * 4;
		r.drawElementsInstancedANGLE(this.gl_.TRIANGLES, l, this.gl_.UNSIGNED_INT, h, i);
		for (let c = 0; c < this.maxAttributeCount_; c++) r.vertexAttribDivisorANGLE(c, 0);
	}
	finalizeDraw(e, t, i) {
		for (let s = 0, r = this.postProcessPasses_.length; s < r; s++) s === r - 1 ? this.postProcessPasses_[s].apply(e, null, t, i) : this.postProcessPasses_[s].apply(e, this.postProcessPasses_[s + 1]);
	}
	getCanvas() {
		return this.gl_.canvas;
	}
	getGL() {
		return this.gl_;
	}
	applyFrameState(e) {
		const t = e.size, i = e.viewState.rotation, s = e.pixelRatio;
		this.setUniformFloatValue(Vt.TIME, (Date.now() - this.startTime_) * .001);
		this.setUniformFloatValue(Vt.ZOOM, e.viewState.zoom);
		this.setUniformFloatValue(Vt.RESOLUTION, e.viewState.resolution);
		this.setUniformFloatValue(Vt.PIXEL_RATIO, e.pixelRatio);
		this.setUniformFloatVec2(Vt.VIEWPORT_SIZE_PX, [e.size[0], e.size[1]]);
		this.setUniformFloatValue(Vt.ROTATION, e.viewState.rotation);
	}
	applyHitDetectionUniform(e) {
		const t = this.getUniformLocation(Vt.HIT_DETECTION);
		this.getGL().uniform1i(t, e ? 1 : 0);
		if (e) {
			this.setUniformFloatValue(Vt.PIXEL_RATIO, .5);
		}
	}
	applyUniforms(e) {
		const t = this.gl_;
		let i, s = 0;
		this.uniforms_.forEach((r) => {
			i = typeof r.value == "function" ? r.value(e) : r.value;
			if (i instanceof HTMLCanvasElement || i instanceof HTMLImageElement || i instanceof ImageData || i instanceof WebGLTexture) {
				i instanceof WebGLTexture && !r.texture ? (r.prevValue = "undefined", r.texture = i) : r.texture || (r.prevValue = "undefined", r.texture = t.createTexture());
				this.bindTexture(r.texture, s, r.name);
				t.texParameteri(t.TEXTURE_2D, t.TEXTURE_MIN_FILTER, t.LINEAR);
				t.texParameteri(t.TEXTURE_2D, t.TEXTURE_WRAP_S, t.CLAMP_TO_EDGE);
				t.texParameteri(t.TEXTURE_2D, t.TEXTURE_WRAP_T, t.CLAMP_TO_EDGE);
				const o = !(i instanceof HTMLImageElement) || i.complete;
				if (!(i instanceof WebGLTexture) && o && r.prevValue !== i) {
					r.prevValue = i;
					t.texImage2D(t.TEXTURE_2D, 0, t.RGBA, t.RGBA, t.UNSIGNED_BYTE, i);
				}
				s++;
			} else if (Array.isArray(i) && i.length === 6) this.setUniformMatrixValue(r.name, za(this.tmpMat4_, i));
			else if (Array.isArray(i) && i.length <= 4) switch (i.length) {
				case 2:
					t.uniform2f(this.getUniformLocation(r.name), i[0], i[1]);
					return;
				case 3:
					t.uniform3f(this.getUniformLocation(r.name), i[0], i[1], i[2]);
					return;
				case 4:
					t.uniform4f(this.getUniformLocation(r.name), i[0], i[1], i[2], i[3]);
					return;
				default: return;
			}
			else typeof i == "number" && t.uniform1f(this.getUniformLocation(r.name), i);
		});
	}
	useProgram(e, t) {
		this.disableAllAttributes_();
		this.gl_.useProgram(e);
		this.currentProgram_ = e;
		if (t) {
			this.applyFrameState(t);
			this.applyUniforms(t);
		}
	}
	compileShader(e, t) {
		const i = this.gl_, s = this.gl_.createShader(t);
		this.gl_.shaderSource(s, e);
		this.gl_.compileShader(s);
		return s;
	}
	getProgram(e, t) {
		const i = this.gl_, s = this.compileShader(e, this.gl_.FRAGMENT_SHADER), r = this.compileShader(t, this.gl_.VERTEX_SHADER), o = this.gl_.createProgram();
		this.gl_.attachShader(o, s);
		this.gl_.attachShader(o, r);
		this.gl_.linkProgram(o);
		if (!this.gl_.getShaderParameter(s, this.gl_.COMPILE_STATUS)) {
			const a = "Fragment shader compilation failed: ".concat(i.getShaderInfoLog(s));
			throw new Error(a);
		}
		this.gl_.deleteShader(s);
		if (!this.gl_.getShaderParameter(r, this.gl_.COMPILE_STATUS)) {
			const a = "Vertex shader compilation failed: ".concat(i.getShaderInfoLog(r));
			throw new Error(a);
		}
		this.gl_.deleteShader(r);
		if (!this.gl_.getProgramParameter(o, this.gl_.LINK_STATUS)) {
			const a = "GL program linking failed: ".concat(i.getProgramInfoLog(o));
			throw new Error(a);
		}
		return o;
	}
	getUniformLocation(e) {
		const t = O(this.currentProgram_);
		if (this.uniformLocationsByProgram_[t] === "undefined") {
			this.uniformLocationsByProgram_[t] = {};
		}
		if (this.uniformLocationsByProgram_[t][e] === "undefined") {
			this.uniformLocationsByProgram_[t][e] = this.gl_.getUniformLocation(this.currentProgram_, e);
		}
		return this.uniformLocationsByProgram_[t][e];
	}
	getAttributeLocation(e) {
		const t = O(this.currentProgram_);
		if (this.attribLocationsByProgram_[t] === "undefined") {
			this.attribLocationsByProgram_[t] = {};
		}
		if (this.attribLocationsByProgram_[t][e] === "undefined") {
			this.attribLocationsByProgram_[t][e] = this.gl_.getAttribLocation(this.currentProgram_, e);
		}
		return this.attribLocationsByProgram_[t][e];
	}
	makeProjectionTransform(e, t) {
		const i = e.size, s = e.viewState.rotation, r = e.viewState.resolution, o = e.viewState.center;
		gt(t, 0, 0, 2 / (e.viewState.resolution * e.size[0]), 2 / (e.viewState.resolution * e.size[1]), -e.viewState.rotation, -e.viewState.center[0], -e.viewState.center[1]);
		return t;
	}
	setUniformFloatValue(e, t) {
		this.gl_.uniform1f(this.getUniformLocation(e), t);
	}
	setUniformFloatVec2(e, t) {
		this.gl_.uniform2fv(this.getUniformLocation(e), t);
	}
	setUniformFloatVec4(e, t) {
		this.gl_.uniform4fv(this.getUniformLocation(e), t);
	}
	setUniformMatrixValue(e, t) {
		this.gl_.uniformMatrix4fv(this.getUniformLocation(e), false, t);
	}
	disableAllAttributes_() {
		for (let e = 0; e < this.maxAttributeCount_; e++) this.gl_.disableVertexAttribArray(e);
	}
	enableAttributeArray_(e, t, i, s, r, o) {
		const a = this.getAttributeLocation(e);
		if (!(a < 0)) {
			this.gl_.enableVertexAttribArray(a);
			this.gl_.vertexAttribPointer(a, t, i, false, s, r);
			if (o) {
				this.getInstancedRenderingExtension_().vertexAttribDivisorANGLE(a, 1);
			}
		}
	}
	enableAttributes_(e, t) {
		const i = BT(e);
		let s = 0;
		for (let r = 0; r < e.length; r++) {
			const o = e[r];
			if (e[r].name) {
				this.enableAttributeArray_(e[r].name, e[r].size, e[r].type || Qd, i, s, t);
			}
			s += e[r].size * ef(e[r].type);
		}
	}
	enableAttributes(e) {
		this.enableAttributes_(e, false);
	}
	enableAttributesInstanced(e) {
		this.enableAttributes_(e, true);
	}
	handleWebGLContextLost(e) {
		Wi(this.bufferCache_);
		this.currentProgram_ = null;
		e.preventDefault();
	}
	handleWebGLContextRestored() {
		this.needsToBeRecreated_ = true;
	}
	needsToBeRecreated() {
		return this.needsToBeRecreated_;
	}
	createTexture(e, t, i, s) {
		const r = this.gl_;
		i = i || this.gl_.createTexture();
		const o = s ? this.gl_.NEAREST : this.gl_.LINEAR;
		this.gl_.bindTexture(this.gl_.TEXTURE_2D, i);
		this.gl_.texParameteri(this.gl_.TEXTURE_2D, this.gl_.TEXTURE_MIN_FILTER, o);
		this.gl_.texParameteri(this.gl_.TEXTURE_2D, this.gl_.TEXTURE_MAG_FILTER, o);
		this.gl_.texParameteri(this.gl_.TEXTURE_2D, this.gl_.TEXTURE_WRAP_S, this.gl_.CLAMP_TO_EDGE);
		this.gl_.texParameteri(this.gl_.TEXTURE_2D, this.gl_.TEXTURE_WRAP_T, this.gl_.CLAMP_TO_EDGE);
		const a = 0, l = this.gl_.RGBA, h = 0, c = this.gl_.RGBA, u = this.gl_.UNSIGNED_BYTE;
		t instanceof Uint8Array ? this.gl_.texImage2D(this.gl_.TEXTURE_2D, 0, this.gl_.RGBA, e[0], e[1], 0, this.gl_.RGBA, this.gl_.UNSIGNED_BYTE, t) : t ? this.gl_.texImage2D(this.gl_.TEXTURE_2D, 0, this.gl_.RGBA, this.gl_.RGBA, this.gl_.UNSIGNED_BYTE, t) : this.gl_.texImage2D(this.gl_.TEXTURE_2D, 0, this.gl_.RGBA, e[0], e[1], 0, this.gl_.RGBA, this.gl_.UNSIGNED_BYTE, null);
		return i;
	}
}
function BT(n) {
	let e = 0;
	for (let t = 0; t < n.length; t++) {
		const i = n[t];
		e += n[t].size * ef(n[t].type);
	}
	return e;
}
function ef(n) {
	switch (n) {
		case Oe.UNSIGNED_BYTE: return Uint8Array.BYTES_PER_ELEMENT;
		case Oe.UNSIGNED_SHORT: return Uint16Array.BYTES_PER_ELEMENT;
		case Oe.UNSIGNED_INT: return Uint32Array.BYTES_PER_ELEMENT;
		case Oe.FLOAT:
		default: return Float32Array.BYTES_PER_ELEMENT;
	}
}
class fh extends wm {
	constructor(e, t) {
		super(e);
		t = t || {};
		this.inversePixelTransform_ = Se();
		this.postProcesses_ = t.postProcesses;
		this.uniforms_ = t.uniforms;
		this.helper;
		this.onMapChanged_ = () => {
			this.clearCache();
			this.removeHelper();
		};
		e.addChangeListener(le.MAP, this.onMapChanged_);
		this.dispatchPreComposeEvent = this.dispatchPreComposeEvent.bind(this);
		this.dispatchPostComposeEvent = this.dispatchPostComposeEvent.bind(this);
	}
	dispatchPreComposeEvent(e, t) {
		const i = this.getLayer();
		if (i.hasListener(Me.PRECOMPOSE)) {
			const s = new Sm(Me.PRECOMPOSE, "undefined", t, e);
			i.dispatchEvent(s);
		}
	}
	dispatchPostComposeEvent(e, t) {
		const i = this.getLayer();
		if (i.hasListener(Me.POSTCOMPOSE)) {
			const s = new Sm(Me.POSTCOMPOSE, "undefined", t, e);
			i.dispatchEvent(s);
		}
	}
	reset(e) {
		this.uniforms_ = e.uniforms;
		if (this.helper) {
			this.helper.setUniforms(this.uniforms_);
		}
	}
	removeHelper() {
		if (this.helper) {
			this.helper.dispose();
			delete this.helper;
		}
	}
	prepareFrame(e) {
		if (this.getLayer().getRenderSource()) {
			let t = true;
			let i = -1;
			let s;
			for (let o = 0, a = e.layerStatesArray.length; o < a; o++) {
				const l = e.layerStatesArray[o].layer;
				const h = e.layerStatesArray[o].layer.getRenderer();
				if (!(h instanceof fh)) {
					t = true;
					continue;
				}
				const c = e.layerStatesArray[o].layer.getClassName();
				if (t || c !== s) {
					i += 1;
					t = false;
				}
				s = c;
				if (h === this) break;
			}
			const r = "map/" + e.mapId + "/group/" + i;
			if (!this.helper || !this.helper.canvasCacheKeyMatches(r) || this.helper.needsToBeRecreated()) {
				this.removeHelper();
				this.helper = new GT({
					postProcesses: this.postProcesses_,
					uniforms: this.uniforms_,
					canvasCacheKey: r
				});
				if (s) {
					this.helper.getCanvas().className = s;
				}
				this.afterHelperCreated();
			}
		}
		return this.prepareFrameInternal(e);
	}
	afterHelperCreated() {}
	prepareFrameInternal(e) {
		return true;
	}
	clearCache() {}
	disposeInternal() {
		var e;
		this.clearCache();
		this.removeHelper();
		if (!((e = this.getLayer()) == null)) {
			e.removeChangeListener(le.MAP, this.onMapChanged_);
		}
		super.disposeInternal();
	}
	dispatchRenderEvent_(e, t, i) {
		const s = this.getLayer();
		if (s.hasListener(e)) {
			gt(this.inversePixelTransform_, 0, 0, i.pixelRatio, -i.pixelRatio, 0, 0, -i.size[1]);
			const r = new Sm(e, this.inversePixelTransform_, i, t);
			s.dispatchEvent(r);
		}
	}
	preRender(e, t) {
		this.dispatchRenderEvent_(Me.PRERENDER, e, t);
	}
	postRender(e, t) {
		this.dispatchRenderEvent_(Me.POSTRENDER, e, t);
	}
}
const jT = {
	TILE_TRANSFORM: "u_tileTransform",
	TRANSITION_ALPHA: "u_transitionAlpha",
	DEPTH: "u_depth",
	RENDER_EXTENT: "u_renderExtent",
	PATTERN_ORIGIN: "u_patternOrigin",
	RESOLUTION: "u_resolution",
	ZOOM: "u_zoom",
	GLOBAL_ALPHA: "u_globalAlpha",
	PROJECTION_MATRIX: "u_projectionMatrix",
	SCREEN_TO_WORLD_MATRIX: "u_screenToWorldMatrix"
};
const da = {
	...jT,
	TILE_TEXTURE_ARRAY: "u_tileTextures",
	TEXTURE_PIXEL_WIDTH: "u_texturePixelWidth",
	TEXTURE_PIXEL_HEIGHT: "u_texturePixelHeight",
	TEXTURE_RESOLUTION: "u_textureResolution",
	TEXTURE_ORIGIN_X: "u_textureOriginX",
	TEXTURE_ORIGIN_Y: "u_textureOriginY"
};
class zT {
	constructor(e, t) {
		this.name = e;
		this.data = t;
		this.texture_ = null;
	}
	getTexture(e) {
		if (!this.texture_) {
			const t = e.createTexture();
			e.bindTexture(e.TEXTURE_2D, t);
			e.texParameteri(e.TEXTURE_2D, e.TEXTURE_WRAP_S, e.CLAMP_TO_EDGE);
			e.texParameteri(e.TEXTURE_2D, e.TEXTURE_WRAP_T, e.CLAMP_TO_EDGE);
			e.texParameteri(e.TEXTURE_2D, e.TEXTURE_MIN_FILTER, e.NEAREST);
			e.texParameteri(e.TEXTURE_2D, e.TEXTURE_MAG_FILTER, e.NEAREST);
			e.texImage2D(e.TEXTURE_2D, 0, e.RGBA, this.data.length / 4, 1, 0, e.RGBA, e.UNSIGNED_BYTE, this.data);
			this.texture_ = t;
		}
		return this.texture_;
	}
	delete(e) {
		if (this.texture_) {
			e.deleteTexture(this.texture_);
		}
		this.texture_ = null;
	}
}
function WT(n, e) {
	return "operator_".concat(n, "_").concat(Object.keys(e.functions).length);
}
function Ri(n) {
	const e = n.toString();
	return e.includes(".") ? e : e + ".0";
}
function gh(n) {
	if (n.length < 2 || n.length > 4) throw new Error("`formatArray` can only output `vec2`, `vec3` or `vec4` arrays.");
	return "vec".concat(n.length, "(").concat(n.map(Ri).join(", "), ")");
}
function vr(n) {
	const e = _t(n), t = e.length > 3 ? e[3] : 1;
	return gh([
		e[0] / 255,
		e[1] / 255,
		e[2] / 255,
		t
	]);
}
function VT(n) {
	const e = Ne(n);
	return gh(e);
}
const fa = {};
let YT = 0;
function Ps(n) {
	if (!(n in fa)) {
		fa[n] = YT++;
	}
	return fa[n];
}
function Ht(n) {
	return Ri(Ps(n));
}
function _h(n) {
	return "u_var_" + n;
}
function tf() {
	return {
		variables: {},
		properties: {},
		functions: {},
		bandCount: 0,
		featureId: false,
		geometryType: false
	};
}
const ga = "getBandValue";
const ZT = "u_paletteTextures";
const nf = "featureId";
const sf = "geometryType";
const Wa = -9999999;
function KT(n, e, t, i) {
	const s = be(n, e, t);
	return mh(s, e, i);
}
function ne(n) {
	return (e, t, i) => {
		const s = t.args.length, r = new Array(t.args.length);
		for (let o = 0; o < t.args.length; ++o) r[o] = mh(t.args[o], i, e);
		return n(r, e);
	};
}
const HT = {
	[C.Get]: (n, e) => {
		const i = e.args[0].value;
		if (!(e.args[0].value in n.properties)) {
			n.properties[e.args[0].value] = {
				name: e.args[0].value,
				type: e.type
			};
		}
		let r = "a_prop_" + e.args[0].value;
		if (e.type === we) {
			r = "(".concat(r, " > 0.0)");
		}
		return r;
	},
	[C.Id]: (n) => n.featureId = true,
	[C.GeometryType]: (n) => n.geometryType = true,
	[C.LineMetric]: () => "currentLineMetric",
	[C.Var]: (n, e) => {
		const i = e.args[0].value;
		if (!(e.args[0].value in n.variables)) {
			n.variables[e.args[0].value] = {
				name: e.args[0].value,
				type: e.type
			};
		}
		let r = _h(e.args[0].value);
		if (e.type === we) {
			r = "(".concat(r, " > 0.0)");
		}
		return r;
	},
	[C.Has]: (n, e) => {
		const i = e.args[0].value;
		if (!(e.args[0].value in n.properties)) {
			n.properties[e.args[0].value] = {
				name: e.args[0].value,
				type: e.type
			};
		}
		return "(a_prop_".concat(e.args[0].value, " != ").concat(Ri(Wa), ")");
	},
	[C.Resolution]: () => "u_resolution",
	[C.Zoom]: () => "u_zoom",
	[C.Time]: () => "u_time",
	[C.Any]: ne((n) => "(".concat(n.join(" || "), ")")),
	[C.All]: ne((n) => "(".concat(n.join(" && "), ")")),
	[C.Not]: ne(([n]) => "(!".concat(n, ")")),
	[C.Equal]: ne(([n, e]) => "(".concat(n, " == ").concat(e, ")")),
	[C.NotEqual]: ne(([n, e]) => "(".concat(n, " != ").concat(e, ")")),
	[C.GreaterThan]: ne(([n, e]) => "(".concat(n, " > ").concat(e, ")")),
	[C.GreaterThanOrEqualTo]: ne(([n, e]) => "(".concat(n, " >= ").concat(e, ")")),
	[C.LessThan]: ne(([n, e]) => "(".concat(n, " < ").concat(e, ")")),
	[C.LessThanOrEqualTo]: ne(([n, e]) => "(".concat(n, " <= ").concat(e, ")")),
	[C.Multiply]: ne((n) => "(".concat(n.join(" * "), ")")),
	[C.Divide]: ne(([n, e]) => "(".concat(n, " / ").concat(e, ")")),
	[C.Add]: ne((n) => "(".concat(n.join(" + "), ")")),
	[C.Subtract]: ne(([n, e]) => "(".concat(n, " - ").concat(e, ")")),
	[C.Clamp]: ne(([n, e, t]) => "clamp(".concat(n, ", ").concat(e, ", ").concat(t, ")")),
	[C.Mod]: ne(([n, e]) => "mod(".concat(n, ", ").concat(e, ")")),
	[C.Pow]: ne(([n, e]) => "pow(".concat(n, ", ").concat(e, ")")),
	[C.Abs]: ne(([n]) => "abs(".concat(n, ")")),
	[C.Floor]: ne(([n]) => "floor(".concat(n, ")")),
	[C.Ceil]: ne(([n]) => "ceil(".concat(n, ")")),
	[C.Round]: ne(([n]) => "floor(".concat(n, " + 0.5)")),
	[C.Sin]: ne(([n]) => "sin(".concat(n, ")")),
	[C.Cos]: ne(([n]) => "cos(".concat(n, ")")),
	[C.Atan]: ne(([n, e]) => e !== "undefined" ? "atan(".concat(n, ", ").concat(e, ")") : "atan(".concat(n, ")")),
	[C.Sqrt]: ne(([n]) => "sqrt(".concat(n, ")")),
	[C.Match]: ne((n) => {
		const e = n[0], t = n[n.length - 1];
		let i = null;
		for (let s = n.length - 3; s >= 1; s -= 2) {
			const r = n[s];
			const o = n[s + 1];
			i = "(".concat(e, " == ").concat(n[s], " ? ").concat(n[s + 1], " : ").concat(i || t, ")");
		}
		return i;
	}),
	[C.Between]: ne(([n, e, t]) => "(".concat(n, " >= ").concat(e, " && ").concat(n, " <= ").concat(t, ")")),
	[C.Interpolate]: ne(([n, e, ...t]) => {
		let i = "";
		for (let s = 0; s < t.length - 2; s += 2) {
			const r = t[s];
			const o = i || t[s + 1];
			const a = t[s + 2];
			const l = t[s + 3];
			let h;
			n === Ri(1) ? h = "(".concat(e, " - ").concat(t[s], ") / (").concat(t[s + 2], " - ").concat(t[s], ")") : h = "(pow(".concat(n, ", (").concat(e, " - ").concat(t[s], ")) - 1.0) / (pow(").concat(n, ", (").concat(t[s + 2], " - ").concat(t[s], ")) - 1.0)");
			i = "mix(".concat(o, ", ").concat(t[s + 3], ", clamp(").concat(h, ", 0.0, 1.0))");
		}
		return i;
	}),
	[C.Case]: ne((n) => {
		const e = n[n.length - 1];
		let t = null;
		for (let i = n.length - 3; i >= 0; i -= 2) {
			const s = n[i];
			const r = n[i + 1];
			t = "(".concat(n[i], " ? ").concat(n[i + 1], " : ").concat(t || e, ")");
		}
		return t;
	}),
	[C.In]: ne(([n, ...e], t) => {
		const i = WT("in", t), s = [];
		for (let r = 0; r < e.length; r += 1) s.push("  if (inputValue == ".concat(e[r], ") { return true; }"));
		t.functions[i] = "bool ".concat(i, "(float inputValue) {\n").concat(s.join("\n"), "\n  return false;\n}");
		return "".concat(i, "(").concat(n, ")");
	}),
	[C.Array]: ne((n) => "vec".concat(n.length, "(").concat(n.join(", "), ")")),
	[C.Color]: ne((n) => {
		if (n.length === 1) return "vec4(vec3(".concat(n[0], " / 255.0), 1.0)");
		if (n.length === 2) return "vec4(vec3(".concat(n[0], " / 255.0), ").concat(n[1], ")");
		const e = n.slice(0, 3).map((i) => "".concat(i, " / 255.0"));
		if (n.length === 3) return "vec4(".concat(e.join(", "), ", 1.0)");
		const t = n[3];
		return "vec4(".concat(e.join(", "), ", ").concat(n[3], ")");
	}),
	[C.Band]: ne(([n, e, t], i) => {
		if (!(ga in i.functions)) {
			let s = "";
			const r = i.bandCount || 1;
			for (let o = 0; o < r; o++) {
				const a = Math.floor(o / 4);
				let l = o % 4;
				if (o === r - 1 && l === 1) {
					l = 3;
				}
				const h = "".concat(da.TILE_TEXTURE_ARRAY, "[").concat(a, "]");
				s += "  if (band == ".concat(o + 1, ".0) {\n    return texture2D(").concat(h, ", v_textureCoord + vec2(dx, dy))[").concat(l, "];\n  }\n");
			}
			i.functions[ga] = "float getBandValue(float band, float xOffset, float yOffset) {\n  float dx = xOffset / ".concat(da.TEXTURE_PIXEL_WIDTH, ";\n  float dy = yOffset / ").concat(da.TEXTURE_PIXEL_HEIGHT, ";\n").concat(s, "\n}");
		}
		return "".concat(ga, "(").concat(n, ", ").concat(e != null ? e : "0.0", ", ").concat(t != null ? t : "0.0", ")");
	}),
	[C.Palette]: (n, e) => {
		const [t, ...i] = e.args, s = i.length, r = new Uint8Array(i.length * 4);
		for (let h = 0; h < i.length; h++) {
			const c = i[h].value;
			const u = _t(i[h].value);
			const d = h * 4;
			r[d] = u[0];
			r[d + 1] = u[1];
			r[d + 2] = u[2];
			r[d + 3] = u[3] * 255;
		}
		if (!n.paletteTextures) {
			n.paletteTextures = [];
		}
		const o = "".concat(ZT, "[").concat(n.paletteTextures.length, "]"), a = new zT(o, r);
		n.paletteTextures.push(a);
		const l = mh(t, G, n);
		return "texture2D(".concat(o, ", vec2((").concat(l, " + 0.5) / ").concat(i.length, ".0, 0.5))");
	}
};
function mh(n, e, t) {
	if (n instanceof nd) {
		const i = HT[n.operator];
		if (HT[n.operator] === "undefined") throw new Error("No compiler defined for this operator: ".concat(JSON.stringify(n.operator)));
		return HT[n.operator](t, n, e);
	}
	if ((n.type & G) > 0) return Ri(n.value);
	if ((n.type & we) > 0) return n.value.toString();
	if ((n.type & Ie) > 0) return Ht(n.value.toString());
	if ((n.type & pe) > 0) return vr(n.value);
	if ((n.type & He) > 0) return gh(n.value);
	if ((n.type & ut) > 0) return VT(n.value);
	throw new Error("Unexpected expression ".concat(n.value, " (expected type ").concat(pn(e), ")"));
}
function qT() {
	return {
		"fill-color": "rgba(255,255,255,0.4)",
		"stroke-color": "#3399CC",
		"stroke-width": 1.25,
		"circle-radius": 5,
		"circle-fill-color": "rgba(255,255,255,0.4)",
		"circle-stroke-width": 1.25,
		"circle-stroke-color": "#3399CC"
	};
}
const Xc = .985;
function Y(n, e, t) {
	const i = Bl();
	return KT(e, t, i, n);
}
function JT(n) {
	const e = _t(n), t = e[0] * 256, i = e[1], s = e[2] * 256, r = Math.round(e[3] * 255);
	return [t + e[1], s + r];
}
function ph(n) {
	return n === pe || n === ut ? 2 : n === He ? 4 : 1;
}
function Va(n) {
	const e = ph(n);
	return e > 1 ? "vec".concat(e) : "float";
}
function rf(n, e) {
	for (const t in e.variables) {
		const i = e.variables[t];
		const s = _h(e.variables[t].name);
		let r = Va(e.variables[t].type);
		if (e.variables[t].type === pe) {
			r = "vec4";
		}
		n.addUniform(s, r);
	}
	for (const t in e.properties) {
		const i = e.properties[t];
		const s = Va(e.properties[t].type);
		const r = "a_prop_".concat(e.properties[t].name);
		e.properties[t].type === pe ? n.addAttribute(r, s, "unpackColor(".concat(r, ")"), "vec4") : n.addAttribute(r, s);
	}
	for (const t in e.functions) {
		n.addVertexShaderFunction(e.functions[t]);
		n.addFragmentShaderFunction(e.functions[t]);
	}
}
function of(n, e) {
	const t = {};
	for (const i in n.variables) {
		const s = n.variables[i];
		const r = _h(n.variables[i].name);
		t[r] = () => {
			var a;
			const o = e[s.name];
			if (typeof e[s.name] == "number") return e[s.name];
			if (typeof e[s.name] == "boolean") return e[s.name] ? 1 : 0;
			if (s.type === pe) {
				const l = [..._t(o || "#eee")];
				l[0] /= 255;
				l[1] /= 255;
				l[2] /= 255;
				if (!((a = l[3]) != null)) {
					l[3] = 1;
				}
				return l;
			}
			return typeof e[s.name] == "string" ? Ps(e[s.name]) : e[s.name];
		};
	}
	return t;
}
function af(n) {
	const e = {};
	for (const t in n.properties) {
		const i = n.properties[t];
		const s = (r) => {
			const o = r.get(i.name);
			return i.type === pe ? JT([..._t(o || "#eee")]) : typeof o == "string" ? Ps(o) : typeof o == "boolean" ? o ? 1 : 0 : o;
		};
		e["prop_".concat(n.properties[t].name)] = {
			size: ph(n.properties[t].type),
			callback: s
		};
	}
	return e;
}
const an = "#ifdef GL_FRAGMENT_PRECISION_HIGH\nprecision highp float;\n#else\nprecision mediump float;\n#endif\nuniform mat4 u_projectionMatrix;\nuniform mat4 u_screenToWorldMatrix;\nuniform vec2 u_viewportSizePx;\nuniform float u_pixelRatio;\nuniform float u_globalAlpha;\nuniform float u_time;\nuniform float u_zoom;\nuniform float u_resolution;\nuniform float u_rotation;\nuniform vec4 u_renderExtent;\nuniform vec2 u_patternOrigin;\nuniform float u_depth;\nuniform mediump int u_hitDetection;\n\nconst float PI = 3.141592653589793238;\nconst float TWO_PI = 2.0 * PI;\nfloat currentLineMetric = 0.; // an actual value will be used in the stroke shaders\n\nvec4 unpackColor(vec2 packedColor) {\n  return vec4(\n    min(floor(packedColor[0] / 256.0) / 255.0, 1.0),\n    min(mod(packedColor[0], 256.0) / 255.0, 1.0),\n    min(floor(packedColor[1] / 256.0) / 255.0, 1.0),\n    min(mod(packedColor[1], 256.0) / 255.0, 1.0)\n  );\n}\n";
const ln = qT();
class lf {
	constructor() {
		this.uniforms_ = [];
		this.attributes_ = [];
		this.hasSymbol_ = false;
		this.symbolSizeExpression_ = "vec2(".concat(Ri(ln["circle-radius"]), " + ").concat(Ri(ln["circle-stroke-width"] * .5), ")");
		this.symbolRotationExpression_ = "0.0";
		this.symbolOffsetExpression_ = "vec2(0.0)";
		this.symbolColorExpression_ = vr(ln["circle-fill-color"]);
		this.texCoordExpression_ = "vec4(0.0, 0.0, 1.0, 1.0)";
		this.discardExpression_ = "false";
		this.symbolRotateWithView_ = false;
		this.hasStroke_ = false;
		this.strokeWidthExpression_ = Ri(ln["stroke-width"]);
		this.strokeColorExpression_ = vr(ln["stroke-color"]);
		this.strokeOffsetExpression_ = "0.";
		this.strokeCapExpression_ = Ht("round");
		this.strokeJoinExpression_ = Ht("round");
		this.strokeMiterLimitExpression_ = "10.";
		this.strokeDistanceFieldExpression_ = "-1000.";
		this.strokePatternLengthExpression_ = null;
		this.hasFill_ = false;
		this.fillColorExpression_ = vr(ln["fill-color"]);
		this.vertexShaderFunctions_ = [];
		this.fragmentShaderFunctions_ = [];
	}
	addUniform(e, t) {
		this.uniforms_.push({
			name: e,
			type: t
		});
		return this;
	}
	addAttribute(e, t, i, s) {
		this.attributes_.push({
			name: e,
			type: t,
			varyingName: e.replace(/^a_/, "v_"),
			varyingType: s != null ? s : t,
			varyingExpression: i != null ? i : e
		});
		return this;
	}
	setSymbolSizeExpression(e) {
		this.hasSymbol_ = true;
		this.symbolSizeExpression_ = e;
		return this;
	}
	getSymbolSizeExpression() {
		return this.symbolSizeExpression_;
	}
	setSymbolRotationExpression(e) {
		this.symbolRotationExpression_ = e;
		return this;
	}
	setSymbolOffsetExpression(e) {
		this.symbolOffsetExpression_ = e;
		return this;
	}
	getSymbolOffsetExpression() {
		return this.symbolOffsetExpression_;
	}
	setSymbolColorExpression(e) {
		this.hasSymbol_ = true;
		this.symbolColorExpression_ = e;
		return this;
	}
	getSymbolColorExpression() {
		return this.symbolColorExpression_;
	}
	setTextureCoordinateExpression(e) {
		this.texCoordExpression_ = e;
		return this;
	}
	setFragmentDiscardExpression(e) {
		this.discardExpression_ = e;
		return this;
	}
	getFragmentDiscardExpression() {
		return this.discardExpression_;
	}
	setSymbolRotateWithView(e) {
		this.symbolRotateWithView_ = e;
		return this;
	}
	setStrokeWidthExpression(e) {
		this.hasStroke_ = true;
		this.strokeWidthExpression_ = e;
		return this;
	}
	setStrokeColorExpression(e) {
		this.hasStroke_ = true;
		this.strokeColorExpression_ = e;
		return this;
	}
	getStrokeColorExpression() {
		return this.strokeColorExpression_;
	}
	setStrokeOffsetExpression(e) {
		this.strokeOffsetExpression_ = e;
		return this;
	}
	setStrokeCapExpression(e) {
		this.strokeCapExpression_ = e;
		return this;
	}
	setStrokeJoinExpression(e) {
		this.strokeJoinExpression_ = e;
		return this;
	}
	setStrokeMiterLimitExpression(e) {
		this.strokeMiterLimitExpression_ = e;
		return this;
	}
	setStrokeDistanceFieldExpression(e) {
		this.strokeDistanceFieldExpression_ = e;
		return this;
	}
	setStrokePatternLengthExpression(e) {
		this.strokePatternLengthExpression_ = e;
		return this;
	}
	getStrokePatternLengthExpression() {
		return this.strokePatternLengthExpression_;
	}
	setFillColorExpression(e) {
		this.hasFill_ = true;
		this.fillColorExpression_ = e;
		return this;
	}
	getFillColorExpression() {
		return this.fillColorExpression_;
	}
	addVertexShaderFunction(e) {
		return this.vertexShaderFunctions_.includes(e) ? this : (this.vertexShaderFunctions_.push(e), this);
	}
	addFragmentShaderFunction(e) {
		return this.fragmentShaderFunctions_.includes(e) ? this : (this.fragmentShaderFunctions_.push(e), this);
	}
	getSymbolVertexShader() {
		return this.hasSymbol_ ? "".concat(an, "\n").concat(this.uniforms_.map((e) => "uniform ".concat(e.type, " ").concat(e.name, ";")).join("\n"), "\nattribute vec2 a_position;\nattribute vec2 a_localPosition;\nattribute vec2 a_hitColor;\n\nvarying vec2 v_texCoord;\nvarying vec2 v_quadCoord;\nvarying vec4 v_hitColor;\nvarying vec2 v_centerPx;\nvarying float v_angle;\nvarying vec2 v_quadSizePx;\n\n").concat(this.attributes_.map((e) => "attribute ".concat(e.type, " ").concat(e.name, ";\nvarying ").concat(e.varyingType, " ").concat(e.varyingName, ";")).join("\n"), "\n").concat(this.vertexShaderFunctions_.join("\n"), "\nvec2 pxToScreen(vec2 coordPx) {\n  vec2 scaled = coordPx / u_viewportSizePx / 0.5;\n  return scaled;\n}\n\nvec2 screenToPx(vec2 coordScreen) {\n  return (coordScreen * 0.5 + 0.5) * u_viewportSizePx;\n}\n\nvoid main(void) {\n  v_quadSizePx = ").concat(this.symbolSizeExpression_, ";\n  vec2 halfSizePx = v_quadSizePx * 0.5;\n  vec2 centerOffsetPx = ").concat(this.symbolOffsetExpression_, ";\n  vec2 offsetPx = centerOffsetPx + a_localPosition * halfSizePx * vec2(1., -1.);\n  float angle = ").concat(this.symbolRotationExpression_).concat(this.symbolRotateWithView_ ? " + u_rotation" : "", ";\n  float c = cos(-angle);\n  float s = sin(-angle);\n  offsetPx = vec2(c * offsetPx.x - s * offsetPx.y, s * offsetPx.x + c * offsetPx.y);\n  vec4 center = u_projectionMatrix * vec4(a_position, 0.0, 1.0);\n  gl_Position = center + vec4(pxToScreen(offsetPx), u_depth, 0.);\n  vec4 texCoord = ").concat(this.texCoordExpression_, ";\n  float u = mix(texCoord.s, texCoord.p, a_localPosition.x * 0.5 + 0.5);\n  float v = mix(texCoord.t, texCoord.q, a_localPosition.y * 0.5 + 0.5);\n  v_texCoord = vec2(u, v);\n  v_hitColor = unpackColor(a_hitColor);\n  v_angle = angle;\n  c = cos(-v_angle);\n  s = sin(-v_angle);\n  centerOffsetPx = vec2(c * centerOffsetPx.x - s * centerOffsetPx.y, s * centerOffsetPx.x + c * centerOffsetPx.y);\n  v_centerPx = screenToPx(center.xy) + centerOffsetPx;\n").concat(this.attributes_.map((e) => "  ".concat(e.varyingName, " = ").concat(e.varyingExpression, ";")).join("\n"), "\n}") : null;
	}
	getSymbolFragmentShader() {
		return this.hasSymbol_ ? "".concat(an, "\n").concat(this.uniforms_.map((e) => "uniform ".concat(e.type, " ").concat(e.name, ";")).join("\n"), "\nvarying vec2 v_texCoord;\nvarying vec4 v_hitColor;\nvarying vec2 v_centerPx;\nvarying float v_angle;\nvarying vec2 v_quadSizePx;\n").concat(this.attributes_.map((e) => "varying ".concat(e.varyingType, " ").concat(e.varyingName, ";")).join("\n"), "\n").concat(this.fragmentShaderFunctions_.join("\n"), "\n\nvoid main(void) {\n").concat(this.attributes_.map((e) => "  ".concat(e.varyingType, " ").concat(e.name, " = ").concat(e.varyingName, "; // assign to original attribute name")).join("\n"), "\n  if (").concat(this.discardExpression_, ") { discard; }\n  vec2 coordsPx = gl_FragCoord.xy / u_pixelRatio - v_centerPx; // relative to center\n  float c = cos(v_angle);\n  float s = sin(v_angle);\n  coordsPx = vec2(c * coordsPx.x - s * coordsPx.y, s * coordsPx.x + c * coordsPx.y);\n  gl_FragColor = ").concat(this.symbolColorExpression_, ";\n  gl_FragColor.rgb *= gl_FragColor.a;\n  if (u_hitDetection > 0) {\n    if (gl_FragColor.a < 0.05) { discard; };\n    gl_FragColor = v_hitColor;\n  }\n}") : null;
	}
	getStrokeVertexShader() {
		return this.hasStroke_ ? "".concat(an, "\n").concat(this.uniforms_.map((e) => "uniform ".concat(e.type, " ").concat(e.name, ";")).join("\n"), "\nattribute vec2 a_segmentStart;\nattribute vec2 a_segmentEnd;\nattribute vec2 a_localPosition;\nattribute float a_measureStart;\nattribute float a_measureEnd;\nattribute float a_angleTangentSum;\nattribute float a_distanceLow;\nattribute float a_distanceHigh;\nattribute vec2 a_joinAngles;\nattribute vec2 a_hitColor;\n\nvarying vec2 v_segmentStartPx;\nvarying vec2 v_segmentEndPx;\nvarying float v_angleStart;\nvarying float v_angleEnd;\nvarying float v_width;\nvarying vec4 v_hitColor;\nvarying float v_distancePx;\nvarying float v_measureStart;\nvarying float v_measureEnd;\n\n").concat(this.attributes_.map((e) => "attribute ".concat(e.type, " ").concat(e.name, ";\nvarying ").concat(e.varyingType, " ").concat(e.varyingName, ";")).join("\n"), "\n").concat(this.vertexShaderFunctions_.join("\n"), "\nvec2 worldToPx(vec2 worldPos) {\n  vec4 screenPos = u_projectionMatrix * vec4(worldPos, 0.0, 1.0);\n  return (0.5 * screenPos.xy + 0.5) * u_viewportSizePx;\n}\n\nvec4 pxToScreen(vec2 pxPos) {\n  vec2 screenPos = 2.0 * pxPos / u_viewportSizePx - 1.0;\n  return vec4(screenPos, u_depth, 1.0);\n}\n\nbool isCap(float joinAngle) {\n  return joinAngle < -0.1;\n}\n\nvec2 getJoinOffsetDirection(vec2 normalPx, float joinAngle) {\n  float halfAngle = joinAngle / 2.0;\n  float c = cos(halfAngle);\n  float s = sin(halfAngle);\n  vec2 angleBisectorNormal = vec2(s * normalPx.x + c * normalPx.y, -c * normalPx.x + s * normalPx.y);\n  float length = 1.0 / s;\n  return angleBisectorNormal * length;\n}\n\nvec2 getOffsetPoint(vec2 point, vec2 normal, float joinAngle, float offsetPx) {\n  // if on a cap or the join angle is too high, offset the line along the segment normal\n  if (cos(joinAngle) > 0.998 || isCap(joinAngle)) {\n    return point - normal * offsetPx;\n  }\n  // offset is applied along the inverted normal (positive offset goes \"right\" relative to line direction)\n  return point - getJoinOffsetDirection(normal, joinAngle) * offsetPx;\n}\n\nvoid main(void) {\n  v_angleStart = a_joinAngles.x;\n  v_angleEnd = a_joinAngles.y;\n  float startEndRatio = a_localPosition.x * 0.5 + 0.5;\n  currentLineMetric = mix(a_measureStart, a_measureEnd, startEndRatio);\n  // we're reading the fractional part while keeping the sign (so -4.12 gives -0.12, 3.45 gives 0.45)\n\n  float lineWidth = ").concat(this.strokeWidthExpression_, ";\n  float lineOffsetPx = ").concat(this.strokeOffsetExpression_, ";\n\n  // compute segment start/end in px with offset\n  vec2 segmentStartPx = worldToPx(a_segmentStart);\n  vec2 segmentEndPx = worldToPx(a_segmentEnd);\n  vec2 tangentPx = normalize(segmentEndPx - segmentStartPx);\n  vec2 normalPx = vec2(-tangentPx.y, tangentPx.x);\n  segmentStartPx = getOffsetPoint(segmentStartPx, normalPx, v_angleStart, lineOffsetPx),\n  segmentEndPx = getOffsetPoint(segmentEndPx, normalPx, v_angleEnd, lineOffsetPx);\n\n  // compute current vertex position\n  float normalDir = -1. * a_localPosition.y;\n  float tangentDir = -1. * a_localPosition.x;\n  float angle = mix(v_angleStart, v_angleEnd, startEndRatio);\n  vec2 joinDirection;\n  vec2 positionPx = mix(segmentStartPx, segmentEndPx, startEndRatio);\n  // if angle is too high, do not make a proper join\n  if (cos(angle) > ").concat(Xc, " || isCap(angle)) {\n    joinDirection = normalPx * normalDir - tangentPx * tangentDir;\n  } else {\n    joinDirection = getJoinOffsetDirection(normalPx * normalDir, angle);\n  }\n  positionPx = positionPx + joinDirection * (lineWidth * 0.5 + 1.); // adding 1 pixel for antialiasing\n  gl_Position = pxToScreen(positionPx);\n\n  v_segmentStartPx = segmentStartPx;\n  v_segmentEndPx = segmentEndPx;\n  v_width = lineWidth;\n  v_hitColor = unpackColor(a_hitColor);\n\n  v_distancePx = a_distanceLow / u_resolution - (lineOffsetPx * a_angleTangentSum);\n  float distanceHighPx = a_distanceHigh / u_resolution;\n  ").concat(this.strokePatternLengthExpression_ !== null ? "v_distancePx = mod(v_distancePx, ".concat(this.strokePatternLengthExpression_, ");\n  distanceHighPx = mod(distanceHighPx, ").concat(this.strokePatternLengthExpression_, ");\n  ") : "", "v_distancePx += distanceHighPx;\n\n  v_measureStart = a_measureStart;\n  v_measureEnd = a_measureEnd;\n").concat(this.attributes_.map((e) => "  ".concat(e.varyingName, " = ").concat(e.varyingExpression, ";")).join("\n"), "\n}") : null;
	}
	getStrokeFragmentShader() {
		return this.hasStroke_ ? "".concat(an, "\n").concat(this.uniforms_.map((e) => "uniform ".concat(e.type, " ").concat(e.name, ";")).join("\n"), "\nvarying vec2 v_segmentStartPx;\nvarying vec2 v_segmentEndPx;\nvarying float v_angleStart;\nvarying float v_angleEnd;\nvarying float v_width;\nvarying vec4 v_hitColor;\nvarying float v_distancePx;\nvarying float v_measureStart;\nvarying float v_measureEnd;\n").concat(this.attributes_.map((e) => "varying ".concat(e.varyingType, " ").concat(e.varyingName, ";")).join("\n"), "\n").concat(this.fragmentShaderFunctions_.join("\n"), "\n\nvec2 pxToWorld(vec2 pxPos) {\n  vec2 screenPos = 2.0 * pxPos / u_viewportSizePx - 1.0;\n  return (u_screenToWorldMatrix * vec4(screenPos, 0.0, 1.0)).xy;\n}\n\nbool isCap(float joinAngle) {\n  return joinAngle < -0.1;\n}\n\nfloat segmentDistanceField(vec2 point, vec2 start, vec2 end, float width) {\n  vec2 tangent = normalize(end - start);\n  vec2 normal = vec2(-tangent.y, tangent.x);\n  vec2 startToPoint = point - start;\n  return abs(dot(startToPoint, normal)) - width * 0.5;\n}\n\nfloat buttCapDistanceField(vec2 point, vec2 start, vec2 end) {\n  vec2 startToPoint = point - start;\n  vec2 tangent = normalize(end - start);\n  return dot(startToPoint, -tangent);\n}\n\nfloat squareCapDistanceField(vec2 point, vec2 start, vec2 end, float width) {\n  return buttCapDistanceField(point, start, end) - width * 0.5;\n}\n\nfloat roundCapDistanceField(vec2 point, vec2 start, vec2 end, float width) {\n  float onSegment = max(0., 1000. * dot(point - start, end - start)); // this is very high when inside the segment\n  return length(point - start) - width * 0.5 - onSegment;\n}\n\nfloat roundJoinDistanceField(vec2 point, vec2 start, vec2 end, float width) {\n  return roundCapDistanceField(point, start, end, width);\n}\n\nfloat bevelJoinField(vec2 point, vec2 start, vec2 end, float width, float joinAngle) {\n  vec2 startToPoint = point - start;\n  vec2 tangent = normalize(end - start);\n  float c = cos(joinAngle * 0.5);\n  float s = sin(joinAngle * 0.5);\n  float direction = -sign(sin(joinAngle));\n  vec2 bisector = vec2(c * tangent.x - s * tangent.y, s * tangent.x + c * tangent.y);\n  float radius = width * 0.5 * s;\n  return dot(startToPoint, bisector * direction) - radius;\n}\n\nfloat miterJoinDistanceField(vec2 point, vec2 start, vec2 end, float width, float joinAngle) {\n  if (cos(joinAngle) > ").concat(Xc, ") { // avoid risking a division by zero\n    return bevelJoinField(point, start, end, width, joinAngle);\n  }\n  float miterLength = 1. / sin(joinAngle * 0.5);\n  float miterLimit = ").concat(this.strokeMiterLimitExpression_, ";\n  if (miterLength > miterLimit) {\n    return bevelJoinField(point, start, end, width, joinAngle);\n  }\n  return -1000.;\n}\n\nfloat capDistanceField(vec2 point, vec2 start, vec2 end, float width, float capType) {\n   if (capType == ").concat(Ht("butt"), ") {\n    return buttCapDistanceField(point, start, end);\n  } else if (capType == ").concat(Ht("square"), ") {\n    return squareCapDistanceField(point, start, end, width);\n  }\n  return roundCapDistanceField(point, start, end, width);\n}\n\nfloat joinDistanceField(vec2 point, vec2 start, vec2 end, float width, float joinAngle, float joinType) {\n  if (joinType == ").concat(Ht("bevel"), ") {\n    return bevelJoinField(point, start, end, width, joinAngle);\n  } else if (joinType == ").concat(Ht("miter"), ") {\n    return miterJoinDistanceField(point, start, end, width, joinAngle);\n  }\n  return roundJoinDistanceField(point, start, end, width);\n}\n\nfloat computeSegmentPointDistance(vec2 point, vec2 start, vec2 end, float width, float joinAngle, float capType, float joinType) {\n  if (isCap(joinAngle)) {\n    return capDistanceField(point, start, end, width, capType);\n  }\n  return joinDistanceField(point, start, end, width, joinAngle, joinType);\n}\n\nfloat distanceFromSegment(vec2 point, vec2 start, vec2 end) {\n  vec2 tangent = end - start;\n  vec2 startToPoint = point - start;\n  // inspire by capsule fn in https://iquilezles.org/articles/distfunctions/\n  float h = clamp(dot(startToPoint, tangent) / dot(tangent, tangent), 0.0, 1.0);\n  return length(startToPoint - tangent * h);\n}\n\nvoid main(void) {\n").concat(this.attributes_.map((e) => "  ".concat(e.varyingType, " ").concat(e.name, " = ").concat(e.varyingName, "; // assign to original attribute name")).join("\n"), "\n\n  vec2 currentPointPx = gl_FragCoord.xy / u_pixelRatio;\n  #ifdef GL_FRAGMENT_PRECISION_HIGH\n  vec2 worldPos = pxToWorld(currentPointPx);\n  if (\n    abs(u_renderExtent[0] - u_renderExtent[2]) > 0.0 && (\n      worldPos[0] < u_renderExtent[0] ||\n      worldPos[1] < u_renderExtent[1] ||\n      worldPos[0] > u_renderExtent[2] ||\n      worldPos[1] > u_renderExtent[3]\n    )\n  ) {\n    discard;\n  }\n  #endif\n\n  float segmentLengthPx = length(v_segmentEndPx - v_segmentStartPx);\n  segmentLengthPx = max(segmentLengthPx, 1.17549429e-38); // avoid divide by zero\n  vec2 segmentTangent = (v_segmentEndPx - v_segmentStartPx) / segmentLengthPx;\n  vec2 segmentNormal = vec2(-segmentTangent.y, segmentTangent.x);\n  vec2 startToPointPx = currentPointPx - v_segmentStartPx;\n  float lengthToPointPx = max(0., min(dot(segmentTangent, startToPointPx), segmentLengthPx));\n  float currentLengthPx = lengthToPointPx + v_distancePx;\n  float currentRadiusPx = distanceFromSegment(currentPointPx, v_segmentStartPx, v_segmentEndPx);\n  float currentRadiusRatio = dot(segmentNormal, startToPointPx) * 2. / v_width;\n  currentLineMetric = mix(v_measureStart, v_measureEnd, lengthToPointPx / segmentLengthPx);\n\n  if (").concat(this.discardExpression_, ") { discard; }\n\n  float capType = ").concat(this.strokeCapExpression_, ";\n  float joinType = ").concat(this.strokeJoinExpression_, ";\n  float segmentStartDistance = computeSegmentPointDistance(currentPointPx, v_segmentStartPx, v_segmentEndPx, v_width, v_angleStart, capType, joinType);\n  float segmentEndDistance = computeSegmentPointDistance(currentPointPx, v_segmentEndPx, v_segmentStartPx, v_width, v_angleEnd, capType, joinType);\n  float distanceField = max(\n    segmentDistanceField(currentPointPx, v_segmentStartPx, v_segmentEndPx, v_width),\n    max(segmentStartDistance, segmentEndDistance)\n  );\n  distanceField = max(distanceField, ").concat(this.strokeDistanceFieldExpression_, ");\n\n  vec4 color = ").concat(this.strokeColorExpression_, ";\n  color.a *= smoothstep(0.5, -0.5, distanceField);\n  gl_FragColor = color;\n  gl_FragColor.a *= u_globalAlpha;\n  gl_FragColor.rgb *= gl_FragColor.a;\n  if (u_hitDetection > 0) {\n    if (gl_FragColor.a < 0.1) { discard; };\n    gl_FragColor = v_hitColor;\n  }\n}") : null;
	}
	getFillVertexShader() {
		return this.hasFill_ ? "".concat(an, "\n").concat(this.uniforms_.map((e) => "uniform ".concat(e.type, " ").concat(e.name, ";")).join("\n"), "\nattribute vec2 a_position;\nattribute vec2 a_hitColor;\n\nvarying vec4 v_hitColor;\n\n").concat(this.attributes_.map((e) => "attribute ".concat(e.type, " ").concat(e.name, ";\nvarying ").concat(e.varyingType, " ").concat(e.varyingName, ";")).join("\n"), "\n").concat(this.vertexShaderFunctions_.join("\n"), "\nvoid main(void) {\n  gl_Position = u_projectionMatrix * vec4(a_position, u_depth, 1.0);\n  v_hitColor = unpackColor(a_hitColor);\n").concat(this.attributes_.map((e) => "  ".concat(e.varyingName, " = ").concat(e.varyingExpression, ";")).join("\n"), "\n}") : null;
	}
	getFillFragmentShader() {
		return this.hasFill_ ? "".concat(an, "\n").concat(this.uniforms_.map((e) => "uniform ".concat(e.type, " ").concat(e.name, ";")).join("\n"), "\nvarying vec4 v_hitColor;\n").concat(this.attributes_.map((e) => "varying ".concat(e.varyingType, " ").concat(e.varyingName, ";")).join("\n"), "\n").concat(this.fragmentShaderFunctions_.join("\n"), "\nvec2 pxToWorld(vec2 pxPos) {\n  vec2 screenPos = 2.0 * pxPos / u_viewportSizePx - 1.0;\n  return (u_screenToWorldMatrix * vec4(screenPos, 0.0, 1.0)).xy;\n}\n\nvec2 worldToPx(vec2 worldPos) {\n  vec4 screenPos = u_projectionMatrix * vec4(worldPos, 0.0, 1.0);\n  return (0.5 * screenPos.xy + 0.5) * u_viewportSizePx;\n}\n\nvoid main(void) {\n").concat(this.attributes_.map((e) => "  ".concat(e.varyingType, " ").concat(e.name, " = ").concat(e.varyingName, "; // assign to original attribute name")).join("\n"), "\n  vec2 pxPos = gl_FragCoord.xy / u_pixelRatio;\n  vec2 pxOrigin = worldToPx(u_patternOrigin);\n  #ifdef GL_FRAGMENT_PRECISION_HIGH\n  vec2 worldPos = pxToWorld(pxPos);\n  if (\n    abs(u_renderExtent[0] - u_renderExtent[2]) > 0.0 && (\n      worldPos[0] < u_renderExtent[0] ||\n      worldPos[1] < u_renderExtent[1] ||\n      worldPos[0] > u_renderExtent[2] ||\n      worldPos[1] > u_renderExtent[3]\n    )\n  ) {\n    discard;\n  }\n  #endif\n  if (").concat(this.discardExpression_, ") { discard; }\n  gl_FragColor = ").concat(this.fillColorExpression_, ";\n  gl_FragColor.a *= u_globalAlpha;\n  gl_FragColor.rgb *= gl_FragColor.a;\n  if (u_hitDetection > 0) {\n    if (gl_FragColor.a < 0.1) { discard; };\n    gl_FragColor = v_hitColor;\n  }\n}") : null;
	}
}
class eo {
	constructor() {
		this.globalCounter_ = 0;
		this.refToFeature_ = new Map();
		this.uidToRef_ = new Map();
		this.freeGlobalRef_ = [];
		this.polygonBatch = {
			entries: {},
			geometriesCount: 0,
			verticesCount: 0,
			ringsCount: 0
		};
		this.pointBatch = {
			entries: {},
			geometriesCount: 0
		};
		this.lineStringBatch = {
			entries: {},
			geometriesCount: 0,
			verticesCount: 0
		};
	}
	addFeatures(e, t) {
		for (let i = 0; i < e.length; i++) this.addFeature(e[i], t);
	}
	addFeature(e, t) {
		let i = e.getGeometry();
		if (i) {
			if (t) {
				i = i.clone();
				i.applyTransform(t);
			}
			this.addGeometry_(i, e);
		}
	}
	clearFeatureEntryInPointBatch_(e) {
		const t = O(e), i = this.pointBatch.entries[t];
		if (this.pointBatch.entries[t]) return this.pointBatch.geometriesCount -= this.pointBatch.entries[t].flatCoordss.length, delete this.pointBatch.entries[t], this.pointBatch.entries[t];
	}
	clearFeatureEntryInLineStringBatch_(e) {
		const t = O(e), i = this.lineStringBatch.entries[t];
		if (this.lineStringBatch.entries[t]) return this.lineStringBatch.verticesCount -= this.lineStringBatch.entries[t].verticesCount, this.lineStringBatch.geometriesCount -= this.lineStringBatch.entries[t].flatCoordss.length, delete this.lineStringBatch.entries[t], this.lineStringBatch.entries[t];
	}
	clearFeatureEntryInPolygonBatch_(e) {
		const t = O(e), i = this.polygonBatch.entries[t];
		if (this.polygonBatch.entries[t]) return this.polygonBatch.verticesCount -= this.polygonBatch.entries[t].verticesCount, this.polygonBatch.ringsCount -= this.polygonBatch.entries[t].ringsCount, this.polygonBatch.geometriesCount -= this.polygonBatch.entries[t].flatCoordss.length, delete this.polygonBatch.entries[t], this.polygonBatch.entries[t];
	}
	addGeometry_(e, t) {
		var s;
		const i = e.getType();
		switch (i) {
			case "GeometryCollection": {
				const r = e.getGeometriesArray();
				for (const o of r) this.addGeometry_(o, t);
				break;
			}
			case "MultiPolygon": {
				const r = e;
				this.addCoordinates_(i, e.getFlatCoordinates(), e.getEndss(), t, O(t), e.getStride());
				break;
			}
			case "MultiLineString": {
				const r = e;
				this.addCoordinates_(i, e.getFlatCoordinates(), e.getEnds(), t, O(t), e.getStride());
				break;
			}
			case "MultiPoint": {
				const r = e;
				this.addCoordinates_(i, e.getFlatCoordinates(), null, t, O(t), e.getStride());
				break;
			}
			case "Polygon": {
				const r = e;
				this.addCoordinates_(i, e.getFlatCoordinates(), e.getEnds(), t, O(t), e.getStride());
				break;
			}
			case "Point": {
				const r = e;
				this.addCoordinates_(i, e.getFlatCoordinates(), null, t, O(t), e.getStride());
				break;
			}
			case "LineString":
			case "LinearRing": {
				const r = e;
				const o = e.getStride();
				this.addCoordinates_(i, e.getFlatCoordinates(), null, t, O(t), o, (s = e.getLayout) == null ? "undefined" : s.call(e));
				break;
			}
		}
	}
	addCoordinates_(e, t, i, s, r, o, a) {
		let l;
		switch (e) {
			case "MultiPolygon": {
				const h = i;
				for (let c = 0, u = i.length; c < u; c++) {
					let d = h[c];
					const f = c > 0 ? h[c - 1] : null;
					const g = f ? f[f.length - 1] : 0;
					const m = d[d.length - 1];
					d = g > 0 ? d.map((_) => _ - g) : d;
					this.addCoordinates_("Polygon", t.slice(g, d[d.length - 1]), d, s, r, o, a);
				}
				break;
			}
			case "MultiLineString": {
				const h = i;
				for (let c = 0, u = i.length; c < u; c++) {
					const d = c > 0 ? h[c - 1] : 0;
					this.addCoordinates_("LineString", t.slice(d, h[c]), null, s, r, o, a);
				}
				break;
			}
			case "MultiPoint":
				for (let h = 0, c = t.length; h < c; h += o) this.addCoordinates_("Point", t.slice(h, h + 2), null, s, r, null, null);
				break;
			case "Polygon": {
				const h = i;
				if (s instanceof Pn) {
					const d = ll(t, h);
					if (d.length > 1) {
						this.addCoordinates_("MultiPolygon", t, d, s, r, o, a);
						return;
					}
				}
				if (!this.polygonBatch.entries[r]) {
					this.polygonBatch.entries[r] = this.addRefToEntry_(r, {
						feature: s,
						flatCoordss: [],
						verticesCount: 0,
						ringsCount: 0,
						ringsVerticesCounts: []
					});
				}
				l = t.length / o;
				const c = i.length;
				const u = i.map((d, f, g) => f > 0 ? (d - g[f - 1]) / o : d / o);
				this.polygonBatch.verticesCount += l;
				this.polygonBatch.ringsCount += i.length;
				this.polygonBatch.geometriesCount++;
				this.polygonBatch.entries[r].flatCoordss.push(eC(t, o));
				this.polygonBatch.entries[r].ringsVerticesCounts.push(u);
				this.polygonBatch.entries[r].verticesCount += l;
				this.polygonBatch.entries[r].ringsCount += i.length;
				for (let d = 0, f = i.length; d < f; d++) {
					const g = d > 0 ? h[d - 1] : 0;
					this.addCoordinates_("LinearRing", t.slice(g, h[d]), null, s, r, o, a);
				}
				break;
			}
			case "Point":
				this.pointBatch.entries[r] || (this.pointBatch.entries[r] = this.addRefToEntry_(r, {
					feature: s,
					flatCoordss: []
				})), this.pointBatch.geometriesCount++, this.pointBatch.entries[r].flatCoordss.push(t);
				break;
			case "LineString":
			case "LinearRing":
				this.lineStringBatch.entries[r] || (this.lineStringBatch.entries[r] = this.addRefToEntry_(r, {
					feature: s,
					flatCoordss: [],
					verticesCount: 0
				})), l = t.length / o, this.lineStringBatch.verticesCount += l, this.lineStringBatch.geometriesCount++, this.lineStringBatch.entries[r].flatCoordss.push(tC(t, o, a)), this.lineStringBatch.entries[r].verticesCount += l;
				break;
		}
	}
	addRefToEntry_(e, t) {
		const i = this.uidToRef_.get(e), s = i || this.freeGlobalRef_.pop() || ++this.globalCounter_;
		t.ref = s;
		if (!i) {
			this.refToFeature_.set(s, t.feature);
			this.uidToRef_.set(e, s);
		}
		return t;
	}
	removeRef_(e, t) {
		if (!e) throw new Error("This feature has no ref: " + t);
		this.refToFeature_.delete(e);
		this.uidToRef_.delete(t);
		this.freeGlobalRef_.push(e);
	}
	changeFeature(e, t) {
		if (!this.uidToRef_.get(O(e))) return;
		this.removeFeature(e);
		let i = e.getGeometry();
		if (i) {
			if (t) {
				i = i.clone();
				i.applyTransform(t);
			}
			this.addGeometry_(i, e);
		}
	}
	removeFeature(e) {
		let t = this.clearFeatureEntryInPointBatch_(e);
		t = this.clearFeatureEntryInPolygonBatch_(e) || t;
		t = this.clearFeatureEntryInLineStringBatch_(e) || t;
		if (t) {
			this.removeRef_(t.ref, O(t.feature));
		}
	}
	clear() {
		this.polygonBatch.entries = {};
		this.polygonBatch.geometriesCount = 0;
		this.polygonBatch.verticesCount = 0;
		this.polygonBatch.ringsCount = 0;
		this.lineStringBatch.entries = {};
		this.lineStringBatch.geometriesCount = 0;
		this.lineStringBatch.verticesCount = 0;
		this.pointBatch.entries = {};
		this.pointBatch.geometriesCount = 0;
		this.globalCounter_ = 0;
		this.freeGlobalRef_ = [];
		this.refToFeature_.clear();
		this.uidToRef_.clear();
	}
	getFeatureFromRef(e) {
		return this.refToFeature_.get(e);
	}
	isEmpty() {
		return this.globalCounter_ === 0;
	}
	filter(e) {
		const t = new eo();
		t.globalCounter_ = this.globalCounter_;
		t.uidToRef_ = this.uidToRef_;
		t.refToFeature_ = this.refToFeature_;
		let i = true;
		for (const s of this.refToFeature_.values()) e(s) && (t.addFeature(s), i = false);
		return i ? new eo() : t;
	}
}
function eC(n, e) {
	return e === 2 ? n : n.filter((t, i) => i % e < 2);
}
function tC(n, e, t) {
	return e === 3 && t === "XYM" ? n : e === 4 ? n.filter((i, s) => s % e !== 2) : e === 3 ? n.map((i, s) => s % e !== 2 ? i : 0) : new Array(n.length * 1.5).fill(0).map((i, s) => s % 3 === 2 ? 0 : n[Math.round(s / 1.5)]);
}
function nC() {
	const n = "function t(t,n,x=2){const o=n&&n.length,i=o?n[0]*x:t.length;let f=e(t,0,i,x,!0);const l=[];if(!f||f.next===f.prev)return l;let c,y,h;if(o&&(f=function(t,n,r,x){const o=[];for(let r=0,i=n.length;r<i;r++){const f=e(t,n[r]*x,r<i-1?n[r+1]*x:t.length,x,!1);f===f.next&&(f.steiner=!0),o.push(a(f))}o.sort(u);for(let t=0;t<o.length;t++)r=s(o[t],r);return r}(t,n,f,x)),t.length>80*x){c=t[0],y=t[1];let e=c,n=y;for(let r=x;r<i;r+=x){const x=t[r],o=t[r+1];x<c&&(c=x),o<y&&(y=o),x>e&&(e=x),o>n&&(n=o)}h=Math.max(e-c,n-y),h=0!==h?32767/h:0}return r(f,l,x,c,y,h,0),l}function e(t,e,n,r,x){let o;if(x===function(t,e,n,r){let x=0;for(let o=e,i=n-r;o<n;o+=r)x+=(t[i]-t[o])*(t[o+1]+t[i+1]),i=o;return x}(t,e,n,r)>0)for(let x=e;x<n;x+=r)o=d(x/r|0,t[x],t[x+1],o);else for(let x=n-r;x>=e;x-=r)o=d(x/r|0,t[x],t[x+1],o);return o&&b(o,o.next)&&(w(o),o=o.next),o}function n(t,e){if(!t)return t;e||(e=t);let n,r=t;do{if(n=!1,r.steiner||!b(r,r.next)&&0!==v(r.prev,r,r.next))r=r.next;else{if(w(r),r=e=r.prev,r===r.next)break;n=!0}}while(n||r!==e);return e}function r(t,e,u,s,l,a,y){if(!t)return;!y&&a&&function(t,e,n,r){let x=t;do{0===x.z&&(x.z=c(x.x,x.y,e,n,r)),x.prevZ=x.prev,x.nextZ=x.next,x=x.next}while(x!==t);x.prevZ.nextZ=null,x.prevZ=null,function(t){let e,n=1;do{let r,x=t;t=null;let o=null;for(e=0;x;){e++;let i=x,f=0;for(let t=0;t<n&&(f++,i=i.nextZ,i);t++);let u=n;for(;f>0||u>0&&i;)0!==f&&(0===u||!i||x.z<=i.z)?(r=x,x=x.nextZ,f--):(r=i,i=i.nextZ,u--),o?o.nextZ=r:t=r,r.prevZ=o,o=r;x=i}o.nextZ=null,n*=2}while(e>1)}(x)}(t,s,l,a);let h=t;for(;t.prev!==t.next;){const c=t.prev,p=t.next;if(a?o(t,s,l,a):x(t))e.push(c.i,t.i,p.i),w(t),t=p.next,h=p.next;else if((t=p)===h){y?1===y?r(t=i(n(t),e),e,u,s,l,a,2):2===y&&f(t,e,u,s,l,a):r(n(t),e,u,s,l,a,1);break}}}function x(t){const e=t.prev,n=t,r=t.next;if(v(e,n,r)>=0)return!1;const x=e.x,o=n.x,i=r.x,f=e.y,u=n.y,s=r.y,l=Math.min(x,o,i),c=Math.min(f,u,s),a=Math.max(x,o,i),y=Math.max(f,u,s);let p=r.next;for(;p!==e;){if(p.x>=l&&p.x<=a&&p.y>=c&&p.y<=y&&h(x,f,o,u,i,s,p.x,p.y)&&v(p.prev,p,p.next)>=0)return!1;p=p.next}return!0}function o(t,e,n,r){const x=t.prev,o=t,i=t.next;if(v(x,o,i)>=0)return!1;const f=x.x,u=o.x,s=i.x,l=x.y,a=o.y,y=i.y,p=Math.min(f,u,s),b=Math.min(l,a,y),M=Math.max(f,u,s),m=Math.max(l,a,y),A=c(p,b,e,n,r),g=c(M,m,e,n,r);let Z=t.prevZ,d=t.nextZ;for(;Z&&Z.z>=A&&d&&d.z<=g;){if(Z.x>=p&&Z.x<=M&&Z.y>=b&&Z.y<=m&&Z!==x&&Z!==i&&h(f,l,u,a,s,y,Z.x,Z.y)&&v(Z.prev,Z,Z.next)>=0)return!1;if(Z=Z.prevZ,d.x>=p&&d.x<=M&&d.y>=b&&d.y<=m&&d!==x&&d!==i&&h(f,l,u,a,s,y,d.x,d.y)&&v(d.prev,d,d.next)>=0)return!1;d=d.nextZ}for(;Z&&Z.z>=A;){if(Z.x>=p&&Z.x<=M&&Z.y>=b&&Z.y<=m&&Z!==x&&Z!==i&&h(f,l,u,a,s,y,Z.x,Z.y)&&v(Z.prev,Z,Z.next)>=0)return!1;Z=Z.prevZ}for(;d&&d.z<=g;){if(d.x>=p&&d.x<=M&&d.y>=b&&d.y<=m&&d!==x&&d!==i&&h(f,l,u,a,s,y,d.x,d.y)&&v(d.prev,d,d.next)>=0)return!1;d=d.nextZ}return!0}function i(t,e){let r=t;do{const n=r.prev,x=r.next.next;!b(n,x)&&M(n,r,r.next,x)&&g(n,x)&&g(x,n)&&(e.push(n.i,r.i,x.i),w(r),w(r.next),r=t=x),r=r.next}while(r!==t);return n(r)}function f(t,e,x,o,i,f){let u=t;do{let t=u.next.next;for(;t!==u.prev;){if(u.i!==t.i&&p(u,t)){let s=Z(u,t);return u=n(u,u.next),s=n(s,s.next),r(u,e,x,o,i,f,0),void r(s,e,x,o,i,f,0)}t=t.next}u=u.next}while(u!==t)}function u(t,e){let n=t.x-e.x;if(0===n&&(n=t.y-e.y,0===n)){n=(t.next.y-t.y)/(t.next.x-t.x)-(e.next.y-e.y)/(e.next.x-e.x)}return n}function s(t,e){const r=function(t,e){let n=e;const r=t.x,x=t.y;let o,i=-1/0;if(b(t,n))return n;do{if(b(t,n.next))return n.next;if(x<=n.y&&x>=n.next.y&&n.next.y!==n.y){const t=n.x+(x-n.y)*(n.next.x-n.x)/(n.next.y-n.y);if(t<=r&&t>i&&(i=t,o=n.x<n.next.x?n:n.next,t===r))return o}n=n.next}while(n!==e);if(!o)return null;const f=o,u=o.x,s=o.y;let c=1/0;n=o;do{if(r>=n.x&&n.x>=u&&r!==n.x&&y(x<s?r:i,x,u,s,x<s?i:r,x,n.x,n.y)){const e=Math.abs(x-n.y)/(r-n.x);g(n,t)&&(e<c||e===c&&(n.x>o.x||n.x===o.x&&l(o,n)))&&(o=n,c=e)}n=n.next}while(n!==f);return o}(t,e);if(!r)return e;const x=Z(r,t);return n(x,x.next),n(r,r.next)}function l(t,e){return v(t.prev,t,e.prev)<0&&v(e.next,t,t.next)<0}function c(t,e,n,r,x){return(t=1431655765&((t=858993459&((t=252645135&((t=16711935&((t=(t-n)*x|0)|t<<8))|t<<4))|t<<2))|t<<1))|(e=1431655765&((e=858993459&((e=252645135&((e=16711935&((e=(e-r)*x|0)|e<<8))|e<<4))|e<<2))|e<<1))<<1}function a(t){let e=t,n=t;do{(e.x<n.x||e.x===n.x&&e.y<n.y)&&(n=e),e=e.next}while(e!==t);return n}function y(t,e,n,r,x,o,i,f){return(x-i)*(e-f)>=(t-i)*(o-f)&&(t-i)*(r-f)>=(n-i)*(e-f)&&(n-i)*(o-f)>=(x-i)*(r-f)}function h(t,e,n,r,x,o,i,f){return!(t===i&&e===f)&&y(t,e,n,r,x,o,i,f)}function p(t,e){return t.next.i!==e.i&&t.prev.i!==e.i&&!function(t,e){let n=t;do{if(n.i!==t.i&&n.next.i!==t.i&&n.i!==e.i&&n.next.i!==e.i&&M(n,n.next,t,e))return!0;n=n.next}while(n!==t);return!1}(t,e)&&(g(t,e)&&g(e,t)&&function(t,e){let n=t,r=!1;const x=(t.x+e.x)/2,o=(t.y+e.y)/2;do{n.y>o!=n.next.y>o&&n.next.y!==n.y&&x<(n.next.x-n.x)*(o-n.y)/(n.next.y-n.y)+n.x&&(r=!r),n=n.next}while(n!==t);return r}(t,e)&&(v(t.prev,t,e.prev)||v(t,e.prev,e))||b(t,e)&&v(t.prev,t,t.next)>0&&v(e.prev,e,e.next)>0)}function v(t,e,n){return(e.y-t.y)*(n.x-e.x)-(e.x-t.x)*(n.y-e.y)}function b(t,e){return t.x===e.x&&t.y===e.y}function M(t,e,n,r){const x=A(v(t,e,n)),o=A(v(t,e,r)),i=A(v(n,r,t)),f=A(v(n,r,e));return x!==o&&i!==f||(!(0!==x||!m(t,n,e))||(!(0!==o||!m(t,r,e))||(!(0!==i||!m(n,t,r))||!(0!==f||!m(n,e,r)))))}function m(t,e,n){return e.x<=Math.max(t.x,n.x)&&e.x>=Math.min(t.x,n.x)&&e.y<=Math.max(t.y,n.y)&&e.y>=Math.min(t.y,n.y)}function A(t){return t>0?1:t<0?-1:0}function g(t,e){return v(t.prev,t,t.next)<0?v(t,e,t.next)>=0&&v(t,t.prev,e)>=0:v(t,e,t.prev)<0||v(t,t.next,e)<0}function Z(t,e){const n=F(t.i,t.x,t.y),r=F(e.i,e.x,e.y),x=t.next,o=e.prev;return t.next=e,e.prev=t,n.next=x,x.prev=n,r.next=n,n.prev=r,o.next=r,r.prev=o,r}function d(t,e,n,r){const x=F(t,e,n);return r?(x.next=r.next,x.prev=r,r.next.prev=x,r.next=x):(x.prev=x,x.next=x),x}function w(t){t.next.prev=t.prev,t.prev.next=t.next,t.prevZ&&(t.prevZ.nextZ=t.nextZ),t.nextZ&&(t.nextZ.prevZ=t.prevZ)}function F(t,e,n){return{i:t,x:e,y:n,prev:null,next:null,z:0,prevZ:null,nextZ:null,steiner:!1}}function E(t,e,n){const r=Math.sqrt((e[0]-t[0])*(e[0]-t[0])+(e[1]-t[1])*(e[1]-t[1])),x=[(e[0]-t[0])/r,(e[1]-t[1])/r],o=[-x[1],x[0]],i=Math.sqrt((n[0]-t[0])*(n[0]-t[0])+(n[1]-t[1])*(n[1]-t[1])),f=[(n[0]-t[0])/i,(n[1]-t[1])/i];let u=0===r||0===i?0:Math.acos((s=f[0]*x[0]+f[1]*x[1],l=-1,c=1,Math.min(Math.max(s,l),c)));var s,l,c;u=Math.max(u,1e-5);return f[0]*o[0]+f[1]*o[1]>0?u:2*Math.PI-u}function I(t,e){const n=e[0],r=e[1];return e[0]=t[0]*n+t[2]*r+t[4],e[1]=t[1]*n+t[3]*r+t[5],e}function z(t,e){const n=(r=e)[0]*r[3]-r[1]*r[2];var r;!function(t,e){if(!t)throw new Error(e)}(0!==n,\"Transformation matrix cannot be inverted\");const x=e[0],o=e[1],i=e[2],f=e[3],u=e[4],s=e[5];return t[0]=f/n,t[1]=-o/n,t[2]=-i/n,t[3]=x/n,t[4]=(i*s-f*u)/n,t[5]=-(x*s-o*u)/n,t}new Array(6);const B=[],P={vertexAttributesPosition:0,instanceAttributesPosition:0,indicesPosition:0};function N(t,e,n,r,x){const o=t[e++],i=t[e++],f=B;f.length=r;for(let n=0;n<f.length;n++)f[n]=t[e+n];let u=x?x.instanceAttributesPosition:0;return n[u++]=o,n[u++]=i,f.length&&(n.set(f,u),u+=f.length),P.instanceAttributesPosition=u,P}function R(t,e,n,r,x,o,i,f,u,s){const l=[t[e],t[e+1]],c=[t[n],t[n+1]],a=t[e+2],y=t[n+2],h=I(f,[...l]),p=I(f,[...c]);let v=-1,b=-1,M=s;const m=null!==x;if(null!==r){v=E(h,p,I(f,[...[t[r],t[r+1]]])),Math.cos(v)<=.985&&(M+=Math.tan((v-Math.PI)/2))}if(m){b=E(p,h,I(f,[...[t[x],t[x+1]]])),Math.cos(b)<=.985&&(M+=Math.tan((Math.PI-b)/2))}const A=Math.pow(2,24),g=u%A,Z=Math.floor(u/A)*A;return o.push(l[0],l[1],a,c[0],c[1],y,v,b,g,Z,s),o.push(...i),{length:u+Math.sqrt((p[0]-h[0])*(p[0]-h[0])+(p[1]-h[1])*(p[1]-h[1])),angle:M}}function S(e,n,r,x,o){const i=2+o;let f=n;const u=e.slice(f,f+o);f+=o;const s=e[f++];let l=0;const c=new Array(s-1);for(let t=0;t<s;t++)l+=e[f++],t<s-1&&(c[t]=l);const a=e.slice(f,f+2*l),y=t(a,c,2);for(let t=0;t<y.length;t++)x.push(y[t]+r.length/i);for(let t=0;t<a.length;t+=2)r.push(a[t],a[t+1],...u);return f+2*l}const T=\"GENERATE_POLYGON_BUFFERS\",_=\"GENERATE_POINT_BUFFERS\",O=\"GENERATE_LINE_STRING_BUFFERS\",U=self;U.onmessage=t=>{const e=t.data;switch(e.type){case _:{const t=2,n=2,r=e.customAttributesSize,x=n+r,o=new Float32Array(e.renderInstructions),i=o.length/x*(t+r),f=Uint32Array.from([0,1,3,1,2,3]),u=Float32Array.from([-1,-1,1,-1,1,1,-1,1]),s=new Float32Array(i);let l;for(let t=0;t<o.length;t+=x)l=N(o,t,s,r,l);const c=Object.assign({indicesBuffer:f.buffer,vertexAttributesBuffer:u.buffer,instanceAttributesBuffer:s.buffer,renderInstructions:o.buffer},e);U.postMessage(c,[u.buffer,s.buffer,f.buffer,o.buffer]);break}case O:{const t=[],n=e.customAttributesSize,r=3,x=new Float32Array(e.renderInstructions);let o=0;const i=[1,0,0,1,0,0];let f,u;for(z(i,e.renderInstructionsTransform);o<x.length;){u=Array.from(x.slice(o,o+n)),o+=n,f=x[o++];const e=o,s=o+(f-1)*r,l=x[e]===x[s]&&x[e+1]===x[s+1];let c=0,a=0;for(let n=0;n<f-1;n++){let y=null;n>0?y=o+(n-1)*r:l&&(y=s-r);let h=null;n<f-2?h=o+(n+2)*r:l&&(h=e+r);const p=R(x,o+n*r,o+(n+1)*r,y,h,t,u,i,c,a);c=p.length,a=p.angle}o+=f*r}const s=Uint32Array.from([0,1,3,1,2,3]),l=Float32Array.from([-1,-1,1,-1,1,1,-1,1]),c=Float32Array.from(t),a=Object.assign({indicesBuffer:s.buffer,vertexAttributesBuffer:l.buffer,instanceAttributesBuffer:c.buffer,renderInstructions:x.buffer},e);U.postMessage(a,[l.buffer,c.buffer,s.buffer,x.buffer]);break}case T:{const t=[],n=[],r=e.customAttributesSize,x=new Float32Array(e.renderInstructions);let o=0;for(;o<x.length;)o=S(x,o,t,n,r);const i=Uint32Array.from(n),f=Float32Array.from(t),u=Float32Array.from([]),s=Object.assign({indicesBuffer:i.buffer,vertexAttributesBuffer:f.buffer,instanceAttributesBuffer:u.buffer,renderInstructions:x.buffer},e);U.postMessage(s,[f.buffer,u.buffer,i.buffer,x.buffer]);break}}};";
	return new Worker(typeof Blob > "u" ? "data:application/javascript;base64," + Buffer.from(n, "binary").toString("base64") : URL.createObjectURL(new Blob(["function t(t,n,x=2){const o=n&&n.length,i=o?n[0]*x:t.length;let f=e(t,0,i,x,!0);const l=[];if(!f||f.next===f.prev)return l;let c,y,h;if(o&&(f=function(t,n,r,x){const o=[];for(let r=0,i=n.length;r<i;r++){const f=e(t,n[r]*x,r<i-1?n[r+1]*x:t.length,x,!1);f===f.next&&(f.steiner=!0),o.push(a(f))}o.sort(u);for(let t=0;t<o.length;t++)r=s(o[t],r);return r}(t,n,f,x)),t.length>80*x){c=t[0],y=t[1];let e=c,n=y;for(let r=x;r<i;r+=x){const x=t[r],o=t[r+1];x<c&&(c=x),o<y&&(y=o),x>e&&(e=x),o>n&&(n=o)}h=Math.max(e-c,n-y),h=0!==h?32767/h:0}return r(f,l,x,c,y,h,0),l}function e(t,e,n,r,x){let o;if(x===function(t,e,n,r){let x=0;for(let o=e,i=n-r;o<n;o+=r)x+=(t[i]-t[o])*(t[o+1]+t[i+1]),i=o;return x}(t,e,n,r)>0)for(let x=e;x<n;x+=r)o=d(x/r|0,t[x],t[x+1],o);else for(let x=n-r;x>=e;x-=r)o=d(x/r|0,t[x],t[x+1],o);return o&&b(o,o.next)&&(w(o),o=o.next),o}function n(t,e){if(!t)return t;e||(e=t);let n,r=t;do{if(n=!1,r.steiner||!b(r,r.next)&&0!==v(r.prev,r,r.next))r=r.next;else{if(w(r),r=e=r.prev,r===r.next)break;n=!0}}while(n||r!==e);return e}function r(t,e,u,s,l,a,y){if(!t)return;!y&&a&&function(t,e,n,r){let x=t;do{0===x.z&&(x.z=c(x.x,x.y,e,n,r)),x.prevZ=x.prev,x.nextZ=x.next,x=x.next}while(x!==t);x.prevZ.nextZ=null,x.prevZ=null,function(t){let e,n=1;do{let r,x=t;t=null;let o=null;for(e=0;x;){e++;let i=x,f=0;for(let t=0;t<n&&(f++,i=i.nextZ,i);t++);let u=n;for(;f>0||u>0&&i;)0!==f&&(0===u||!i||x.z<=i.z)?(r=x,x=x.nextZ,f--):(r=i,i=i.nextZ,u--),o?o.nextZ=r:t=r,r.prevZ=o,o=r;x=i}o.nextZ=null,n*=2}while(e>1)}(x)}(t,s,l,a);let h=t;for(;t.prev!==t.next;){const c=t.prev,p=t.next;if(a?o(t,s,l,a):x(t))e.push(c.i,t.i,p.i),w(t),t=p.next,h=p.next;else if((t=p)===h){y?1===y?r(t=i(n(t),e),e,u,s,l,a,2):2===y&&f(t,e,u,s,l,a):r(n(t),e,u,s,l,a,1);break}}}function x(t){const e=t.prev,n=t,r=t.next;if(v(e,n,r)>=0)return!1;const x=e.x,o=n.x,i=r.x,f=e.y,u=n.y,s=r.y,l=Math.min(x,o,i),c=Math.min(f,u,s),a=Math.max(x,o,i),y=Math.max(f,u,s);let p=r.next;for(;p!==e;){if(p.x>=l&&p.x<=a&&p.y>=c&&p.y<=y&&h(x,f,o,u,i,s,p.x,p.y)&&v(p.prev,p,p.next)>=0)return!1;p=p.next}return!0}function o(t,e,n,r){const x=t.prev,o=t,i=t.next;if(v(x,o,i)>=0)return!1;const f=x.x,u=o.x,s=i.x,l=x.y,a=o.y,y=i.y,p=Math.min(f,u,s),b=Math.min(l,a,y),M=Math.max(f,u,s),m=Math.max(l,a,y),A=c(p,b,e,n,r),g=c(M,m,e,n,r);let Z=t.prevZ,d=t.nextZ;for(;Z&&Z.z>=A&&d&&d.z<=g;){if(Z.x>=p&&Z.x<=M&&Z.y>=b&&Z.y<=m&&Z!==x&&Z!==i&&h(f,l,u,a,s,y,Z.x,Z.y)&&v(Z.prev,Z,Z.next)>=0)return!1;if(Z=Z.prevZ,d.x>=p&&d.x<=M&&d.y>=b&&d.y<=m&&d!==x&&d!==i&&h(f,l,u,a,s,y,d.x,d.y)&&v(d.prev,d,d.next)>=0)return!1;d=d.nextZ}for(;Z&&Z.z>=A;){if(Z.x>=p&&Z.x<=M&&Z.y>=b&&Z.y<=m&&Z!==x&&Z!==i&&h(f,l,u,a,s,y,Z.x,Z.y)&&v(Z.prev,Z,Z.next)>=0)return!1;Z=Z.prevZ}for(;d&&d.z<=g;){if(d.x>=p&&d.x<=M&&d.y>=b&&d.y<=m&&d!==x&&d!==i&&h(f,l,u,a,s,y,d.x,d.y)&&v(d.prev,d,d.next)>=0)return!1;d=d.nextZ}return!0}function i(t,e){let r=t;do{const n=r.prev,x=r.next.next;!b(n,x)&&M(n,r,r.next,x)&&g(n,x)&&g(x,n)&&(e.push(n.i,r.i,x.i),w(r),w(r.next),r=t=x),r=r.next}while(r!==t);return n(r)}function f(t,e,x,o,i,f){let u=t;do{let t=u.next.next;for(;t!==u.prev;){if(u.i!==t.i&&p(u,t)){let s=Z(u,t);return u=n(u,u.next),s=n(s,s.next),r(u,e,x,o,i,f,0),void r(s,e,x,o,i,f,0)}t=t.next}u=u.next}while(u!==t)}function u(t,e){let n=t.x-e.x;if(0===n&&(n=t.y-e.y,0===n)){n=(t.next.y-t.y)/(t.next.x-t.x)-(e.next.y-e.y)/(e.next.x-e.x)}return n}function s(t,e){const r=function(t,e){let n=e;const r=t.x,x=t.y;let o,i=-1/0;if(b(t,n))return n;do{if(b(t,n.next))return n.next;if(x<=n.y&&x>=n.next.y&&n.next.y!==n.y){const t=n.x+(x-n.y)*(n.next.x-n.x)/(n.next.y-n.y);if(t<=r&&t>i&&(i=t,o=n.x<n.next.x?n:n.next,t===r))return o}n=n.next}while(n!==e);if(!o)return null;const f=o,u=o.x,s=o.y;let c=1/0;n=o;do{if(r>=n.x&&n.x>=u&&r!==n.x&&y(x<s?r:i,x,u,s,x<s?i:r,x,n.x,n.y)){const e=Math.abs(x-n.y)/(r-n.x);g(n,t)&&(e<c||e===c&&(n.x>o.x||n.x===o.x&&l(o,n)))&&(o=n,c=e)}n=n.next}while(n!==f);return o}(t,e);if(!r)return e;const x=Z(r,t);return n(x,x.next),n(r,r.next)}function l(t,e){return v(t.prev,t,e.prev)<0&&v(e.next,t,t.next)<0}function c(t,e,n,r,x){return(t=1431655765&((t=858993459&((t=252645135&((t=16711935&((t=(t-n)*x|0)|t<<8))|t<<4))|t<<2))|t<<1))|(e=1431655765&((e=858993459&((e=252645135&((e=16711935&((e=(e-r)*x|0)|e<<8))|e<<4))|e<<2))|e<<1))<<1}function a(t){let e=t,n=t;do{(e.x<n.x||e.x===n.x&&e.y<n.y)&&(n=e),e=e.next}while(e!==t);return n}function y(t,e,n,r,x,o,i,f){return(x-i)*(e-f)>=(t-i)*(o-f)&&(t-i)*(r-f)>=(n-i)*(e-f)&&(n-i)*(o-f)>=(x-i)*(r-f)}function h(t,e,n,r,x,o,i,f){return!(t===i&&e===f)&&y(t,e,n,r,x,o,i,f)}function p(t,e){return t.next.i!==e.i&&t.prev.i!==e.i&&!function(t,e){let n=t;do{if(n.i!==t.i&&n.next.i!==t.i&&n.i!==e.i&&n.next.i!==e.i&&M(n,n.next,t,e))return!0;n=n.next}while(n!==t);return!1}(t,e)&&(g(t,e)&&g(e,t)&&function(t,e){let n=t,r=!1;const x=(t.x+e.x)/2,o=(t.y+e.y)/2;do{n.y>o!=n.next.y>o&&n.next.y!==n.y&&x<(n.next.x-n.x)*(o-n.y)/(n.next.y-n.y)+n.x&&(r=!r),n=n.next}while(n!==t);return r}(t,e)&&(v(t.prev,t,e.prev)||v(t,e.prev,e))||b(t,e)&&v(t.prev,t,t.next)>0&&v(e.prev,e,e.next)>0)}function v(t,e,n){return(e.y-t.y)*(n.x-e.x)-(e.x-t.x)*(n.y-e.y)}function b(t,e){return t.x===e.x&&t.y===e.y}function M(t,e,n,r){const x=A(v(t,e,n)),o=A(v(t,e,r)),i=A(v(n,r,t)),f=A(v(n,r,e));return x!==o&&i!==f||(!(0!==x||!m(t,n,e))||(!(0!==o||!m(t,r,e))||(!(0!==i||!m(n,t,r))||!(0!==f||!m(n,e,r)))))}function m(t,e,n){return e.x<=Math.max(t.x,n.x)&&e.x>=Math.min(t.x,n.x)&&e.y<=Math.max(t.y,n.y)&&e.y>=Math.min(t.y,n.y)}function A(t){return t>0?1:t<0?-1:0}function g(t,e){return v(t.prev,t,t.next)<0?v(t,e,t.next)>=0&&v(t,t.prev,e)>=0:v(t,e,t.prev)<0||v(t,t.next,e)<0}function Z(t,e){const n=F(t.i,t.x,t.y),r=F(e.i,e.x,e.y),x=t.next,o=e.prev;return t.next=e,e.prev=t,n.next=x,x.prev=n,r.next=n,n.prev=r,o.next=r,r.prev=o,r}function d(t,e,n,r){const x=F(t,e,n);return r?(x.next=r.next,x.prev=r,r.next.prev=x,r.next=x):(x.prev=x,x.next=x),x}function w(t){t.next.prev=t.prev,t.prev.next=t.next,t.prevZ&&(t.prevZ.nextZ=t.nextZ),t.nextZ&&(t.nextZ.prevZ=t.prevZ)}function F(t,e,n){return{i:t,x:e,y:n,prev:null,next:null,z:0,prevZ:null,nextZ:null,steiner:!1}}function E(t,e,n){const r=Math.sqrt((e[0]-t[0])*(e[0]-t[0])+(e[1]-t[1])*(e[1]-t[1])),x=[(e[0]-t[0])/r,(e[1]-t[1])/r],o=[-x[1],x[0]],i=Math.sqrt((n[0]-t[0])*(n[0]-t[0])+(n[1]-t[1])*(n[1]-t[1])),f=[(n[0]-t[0])/i,(n[1]-t[1])/i];let u=0===r||0===i?0:Math.acos((s=f[0]*x[0]+f[1]*x[1],l=-1,c=1,Math.min(Math.max(s,l),c)));var s,l,c;u=Math.max(u,1e-5);return f[0]*o[0]+f[1]*o[1]>0?u:2*Math.PI-u}function I(t,e){const n=e[0],r=e[1];return e[0]=t[0]*n+t[2]*r+t[4],e[1]=t[1]*n+t[3]*r+t[5],e}function z(t,e){const n=(r=e)[0]*r[3]-r[1]*r[2];var r;!function(t,e){if(!t)throw new Error(e)}(0!==n,\"Transformation matrix cannot be inverted\");const x=e[0],o=e[1],i=e[2],f=e[3],u=e[4],s=e[5];return t[0]=f/n,t[1]=-o/n,t[2]=-i/n,t[3]=x/n,t[4]=(i*s-f*u)/n,t[5]=-(x*s-o*u)/n,t}new Array(6);const B=[],P={vertexAttributesPosition:0,instanceAttributesPosition:0,indicesPosition:0};function N(t,e,n,r,x){const o=t[e++],i=t[e++],f=B;f.length=r;for(let n=0;n<f.length;n++)f[n]=t[e+n];let u=x?x.instanceAttributesPosition:0;return n[u++]=o,n[u++]=i,f.length&&(n.set(f,u),u+=f.length),P.instanceAttributesPosition=u,P}function R(t,e,n,r,x,o,i,f,u,s){const l=[t[e],t[e+1]],c=[t[n],t[n+1]],a=t[e+2],y=t[n+2],h=I(f,[...l]),p=I(f,[...c]);let v=-1,b=-1,M=s;const m=null!==x;if(null!==r){v=E(h,p,I(f,[...[t[r],t[r+1]]])),Math.cos(v)<=.985&&(M+=Math.tan((v-Math.PI)/2))}if(m){b=E(p,h,I(f,[...[t[x],t[x+1]]])),Math.cos(b)<=.985&&(M+=Math.tan((Math.PI-b)/2))}const A=Math.pow(2,24),g=u%A,Z=Math.floor(u/A)*A;return o.push(l[0],l[1],a,c[0],c[1],y,v,b,g,Z,s),o.push(...i),{length:u+Math.sqrt((p[0]-h[0])*(p[0]-h[0])+(p[1]-h[1])*(p[1]-h[1])),angle:M}}function S(e,n,r,x,o){const i=2+o;let f=n;const u=e.slice(f,f+o);f+=o;const s=e[f++];let l=0;const c=new Array(s-1);for(let t=0;t<s;t++)l+=e[f++],t<s-1&&(c[t]=l);const a=e.slice(f,f+2*l),y=t(a,c,2);for(let t=0;t<y.length;t++)x.push(y[t]+r.length/i);for(let t=0;t<a.length;t+=2)r.push(a[t],a[t+1],...u);return f+2*l}const T=\"GENERATE_POLYGON_BUFFERS\",_=\"GENERATE_POINT_BUFFERS\",O=\"GENERATE_LINE_STRING_BUFFERS\",U=self;U.onmessage=t=>{const e=t.data;switch(e.type){case _:{const t=2,n=2,r=e.customAttributesSize,x=n+r,o=new Float32Array(e.renderInstructions),i=o.length/x*(t+r),f=Uint32Array.from([0,1,3,1,2,3]),u=Float32Array.from([-1,-1,1,-1,1,1,-1,1]),s=new Float32Array(i);let l;for(let t=0;t<o.length;t+=x)l=N(o,t,s,r,l);const c=Object.assign({indicesBuffer:f.buffer,vertexAttributesBuffer:u.buffer,instanceAttributesBuffer:s.buffer,renderInstructions:o.buffer},e);U.postMessage(c,[u.buffer,s.buffer,f.buffer,o.buffer]);break}case O:{const t=[],n=e.customAttributesSize,r=3,x=new Float32Array(e.renderInstructions);let o=0;const i=[1,0,0,1,0,0];let f,u;for(z(i,e.renderInstructionsTransform);o<x.length;){u=Array.from(x.slice(o,o+n)),o+=n,f=x[o++];const e=o,s=o+(f-1)*r,l=x[e]===x[s]&&x[e+1]===x[s+1];let c=0,a=0;for(let n=0;n<f-1;n++){let y=null;n>0?y=o+(n-1)*r:l&&(y=s-r);let h=null;n<f-2?h=o+(n+2)*r:l&&(h=e+r);const p=R(x,o+n*r,o+(n+1)*r,y,h,t,u,i,c,a);c=p.length,a=p.angle}o+=f*r}const s=Uint32Array.from([0,1,3,1,2,3]),l=Float32Array.from([-1,-1,1,-1,1,1,-1,1]),c=Float32Array.from(t),a=Object.assign({indicesBuffer:s.buffer,vertexAttributesBuffer:l.buffer,instanceAttributesBuffer:c.buffer,renderInstructions:x.buffer},e);U.postMessage(a,[l.buffer,c.buffer,s.buffer,x.buffer]);break}case T:{const t=[],n=[],r=e.customAttributesSize,x=new Float32Array(e.renderInstructions);let o=0;for(;o<x.length;)o=S(x,o,t,n,r);const i=Uint32Array.from(n),f=Float32Array.from(t),u=Float32Array.from([]),s=Object.assign({indicesBuffer:i.buffer,vertexAttributesBuffer:f.buffer,instanceAttributesBuffer:u.buffer,renderInstructions:x.buffer},e);U.postMessage(s,[f.buffer,u.buffer,i.buffer,x.buffer]);break}}};"], { type: "application/javascript" })));
}
const _a = {
	GENERATE_POLYGON_BUFFERS: "GENERATE_POLYGON_BUFFERS",
	GENERATE_POINT_BUFFERS: "GENERATE_POINT_BUFFERS",
	GENERATE_LINE_STRING_BUFFERS: "GENERATE_LINE_STRING_BUFFERS"
};
function sC(n, e) {
	e = e || [];
	const t = 256, i = 255, s = Math.floor(n / 256 / 256 / 256) / 255, r = Math.floor(n / 256 / 256) % 256 / 255, o = Math.floor(n / 256) % 256 / 255, a = n % 256 / 255;
	e[0] = s * 256 * 255 + r * 255;
	e[1] = o * 256 * 255 + a * 255;
	return e;
}
function rC(n) {
	let e = 0;
	const t = 256, i = 255;
	e += Math.round(n[0] * 256 * 256 * 256 * 255);
	e += Math.round(n[1] * 256 * 256 * 255);
	e += Math.round(n[2] * 256 * 255);
	e += Math.round(n[3] * 255);
	return e;
}
function yh(n, e, t, i) {
	var r;
	let s = 0;
	for (const o in e) {
		const a = e[o];
		const l = e[o].callback.call(t, t.feature);
		let h = (r = l == null ? "undefined" : l[0]) != null ? r : l;
		if (h === Wa) {
			console.warn("The \"has\" operator might return false positives.");
		}
		h === "undefined" ? h = Wa : h === null && (h = 0);
		n[i + s++] = h;
		if (!(!e[o].size || e[o].size === 1)) {
			n[i + s++] = l[1];
			if (!(e[o].size < 3)) {
				n[i + s++] = l[2];
				if (!(e[o].size < 4)) {
					n[i + s++] = l[3];
				}
			}
		}
	}
	return s;
}
function Eo(n) {
	return Object.keys(n).reduce((e, t) => e + (n[t].size || 1), 0);
}
function oC(n, e, t, i) {
	const s = (2 + Eo(t)) * n.geometriesCount;
	if (!e || e.length !== s) {
		e = new Float32Array(s);
	}
	const r = [];
	let o = 0;
	for (const a in n.entries) {
		const l = n.entries[a];
		for (let h = 0, c = n.entries[a].flatCoordss.length; h < c; h++) {
			r[0] = n.entries[a].flatCoordss[h][0];
			r[1] = n.entries[a].flatCoordss[h][1];
			xe(i, r);
			e[o++] = r[0];
			e[o++] = r[1];
			o += yh(e, t, n.entries[a], o);
		}
	}
	return e;
}
function aC(n, e, t, i) {
	const s = 3 * n.verticesCount + (1 + Eo(t)) * n.geometriesCount;
	if (!e || e.length !== s) {
		e = new Float32Array(s);
	}
	const r = [];
	let o = 0;
	for (const a in n.entries) {
		const l = n.entries[a];
		for (let h = 0, c = n.entries[a].flatCoordss.length; h < c; h++) {
			r.length = l.flatCoordss[h].length;
			At(l.flatCoordss[h], 0, r.length, 3, i, r, 3);
			o += yh(e, t, l, o);
			e[o++] = r.length / 3;
			for (let u = 0, d = r.length; u < d; u += 3) {
				e[o++] = r[u];
				e[o++] = r[u + 1];
				e[o++] = r[u + 2];
			}
		}
	}
	return e;
}
function lC(n, e, t, i) {
	const s = 2 * n.verticesCount + (1 + Eo(t)) * n.geometriesCount + n.ringsCount;
	if (!e || e.length !== s) {
		e = new Float32Array(s);
	}
	const r = [];
	let o = 0;
	for (const a in n.entries) {
		const l = n.entries[a];
		for (let h = 0, c = n.entries[a].flatCoordss.length; h < c; h++) {
			r.length = l.flatCoordss[h].length;
			At(l.flatCoordss[h], 0, r.length, 2, i, r);
			o += yh(e, t, l, o);
			e[o++] = l.ringsVerticesCounts[h].length;
			for (let u = 0, d = l.ringsVerticesCounts[h].length; u < d; u++) e[o++] = l.ringsVerticesCounts[h][u];
			for (let u = 0, d = r.length; u < d; u += 2) {
				e[o++] = r[u];
				e[o++] = r[u + 1];
			}
		}
	}
	return e;
}
function to(n) {
	return (JSON.stringify(n).split("").reduce((t, i) => (t << 5) - t + i.charCodeAt(0), 0) >>> 0).toString();
}
function xh(n, e, t, i) {
	if ("".concat(i, "radius") in n && i !== "icon-") {
		let s = Y(t, n["".concat(i, "radius")], G);
		if ("".concat(i, "radius2") in n) {
			const r = Y(t, n["".concat(i, "radius2")], G);
			s = "max(".concat(s, ", ").concat(r, ")");
		}
		if ("".concat(i, "stroke-width") in n) {
			s = "(".concat(s, " + ").concat(Y(t, n["".concat(i, "stroke-width")], G), " * 0.5)");
		}
		e.setSymbolSizeExpression("vec2(".concat(s, " * 2. + 0.5)"));
	}
	if ("".concat(i, "scale") in n) {
		const s = Y(t, n["".concat(i, "scale")], ut);
		e.setSymbolSizeExpression("".concat(e.getSymbolSizeExpression(), " * ").concat(s));
	}
	if ("".concat(i, "displacement") in n) {
		e.setSymbolOffsetExpression(Y(t, n["".concat(i, "displacement")], He));
	}
	if ("".concat(i, "rotation") in n) {
		e.setSymbolRotationExpression(Y(t, n["".concat(i, "rotation")], G));
	}
	if ("".concat(i, "rotate-with-view") in n) {
		e.setSymbolRotateWithView(!!n["".concat(i, "rotate-with-view")]);
	}
}
function hf(n, e, t, i, s) {
	let r = "vec4(0.)";
	r = e;
	if (t !== null && i !== null) {
		const l = "smoothstep(-".concat(i, " + 0.63, -").concat(i, " - 0.58, ").concat(n, ")");
		r = "mix(".concat(t, ", ").concat(r, ", ").concat(l, ")");
	}
	const o = "(1.0 - smoothstep(-0.63, 0.58, ".concat(n, "))");
	let a = "".concat(r, " * vec4(1.0, 1.0, 1.0, ").concat(o, ")");
	a = "".concat(a, " * vec4(1.0, 1.0, 1.0, ").concat(s, ")");
	return a;
}
function Eh(n, e, t, i, s) {
	const r = new Image();
	r.crossOrigin = n["".concat(i, "cross-origin")] === "undefined" ? "anonymous" : n["".concat(i, "cross-origin")];
	ee(typeof n["".concat(i, "src")] == "string", "WebGL layers do not support expressions for the ".concat(i, "src style property"));
	r.src = n["".concat(i, "src")];
	t["u_texture".concat(s, "_size")] = () => r.complete ? [r.width, r.height] : [0, 0];
	e.addUniform("u_texture".concat(s, "_size"), "vec2");
	const o = "u_texture".concat(s, "_size");
	t["u_texture".concat(s)] = r;
	e.addUniform("u_texture".concat(s), "sampler2D");
	return o;
}
function Th(n, e, t, i, s) {
	let r = Y(t, n["".concat(e, "offset")], ut);
	if ("".concat(e, "offset-origin") in n) switch (n["".concat(e, "offset-origin")]) {
		case "top-right":
			r = "vec2(".concat(i, ".x, 0.) + ").concat(s, " * vec2(-1., 0.) + ").concat(r, " * vec2(-1., 1.)");
			break;
		case "bottom-left":
			r = "vec2(0., ".concat(i, ".y) + ").concat(s, " * vec2(0., -1.) + ").concat(r, " * vec2(1., -1.)");
			break;
		case "bottom-right":
			r = "".concat(i, " - ").concat(s, " - ").concat(r);
			break;
	}
	return r;
}
function hC(n, e, t, i) {
	i.functions.circleDistanceField = "float circleDistanceField(vec2 point, float radius) {\n  return length(point) - radius;\n}";
	xh(n, e, i, "circle-");
	let s = null;
	if ("circle-opacity" in n) {
		s = Y(i, n["circle-opacity"], G);
	}
	let r = "coordsPx";
	if ("circle-scale" in n) {
		const d = Y(i, n["circle-scale"], ut);
		r = "coordsPx / ".concat(d);
	}
	let o = null;
	if ("circle-fill-color" in n) {
		o = Y(i, n["circle-fill-color"], pe);
	}
	let a = null;
	if ("circle-stroke-color" in n) {
		a = Y(i, n["circle-stroke-color"], pe);
	}
	let l = Y(i, n["circle-radius"], G), h = null;
	if ("circle-stroke-width" in n) {
		h = Y(i, n["circle-stroke-width"], G);
		l = "(".concat(l, " + ").concat(h, " * 0.5)");
	}
	const c = "circleDistanceField(".concat(r, ", ").concat(l, ")"), u = hf(c, o, a, h, s);
	e.setSymbolColorExpression(u);
}
function cC(n, e, t, i) {
	i.functions.round = "float round(float v) {\n  return sign(v) * floor(abs(v) + 0.5);\n}";
	i.functions.starDistanceField = "float starDistanceField(vec2 point, float numPoints, float radius, float radius2, float angle) {\n  float startAngle = -PI * 0.5 + angle; // tip starts upwards and rotates clockwise with angle\n  float c = cos(startAngle);\n  float s = sin(startAngle);\n  vec2 pointRotated = vec2(c * point.x - s * point.y, s * point.x + c * point.y);\n  float alpha = TWO_PI / numPoints; // the angle of one sector\n  float beta = atan(pointRotated.y, pointRotated.x);\n  float gamma = round(beta / alpha) * alpha; // angle in sector\n  c = cos(-gamma);\n  s = sin(-gamma);\n  vec2 inSector = vec2(c * pointRotated.x - s * pointRotated.y, abs(s * pointRotated.x + c * pointRotated.y));\n  vec2 tipToPoint = inSector + vec2(-radius, 0.);\n  vec2 edgeNormal = vec2(radius2 * sin(alpha * 0.5), -radius2 * cos(alpha * 0.5) + radius);\n  return dot(normalize(edgeNormal), tipToPoint);\n}";
	i.functions.regularDistanceField = "float regularDistanceField(vec2 point, float numPoints, float radius, float angle) {\n  float startAngle = -PI * 0.5 + angle; // tip starts upwards and rotates clockwise with angle\n  float c = cos(startAngle);\n  float s = sin(startAngle);\n  vec2 pointRotated = vec2(c * point.x - s * point.y, s * point.x + c * point.y);\n  float alpha = TWO_PI / numPoints; // the angle of one sector\n  float radiusIn = radius * cos(PI / numPoints);\n  float beta = atan(pointRotated.y, pointRotated.x);\n  float gamma = round((beta - alpha * 0.5) / alpha) * alpha + alpha * 0.5; // angle in sector from mid\n  c = cos(-gamma);\n  s = sin(-gamma);\n  vec2 inSector = vec2(c * pointRotated.x - s * pointRotated.y, abs(s * pointRotated.x + c * pointRotated.y));\n  return inSector.x - radiusIn;\n}";
	xh(n, e, i, "shape-");
	let s = null;
	if ("shape-opacity" in n) {
		s = Y(i, n["shape-opacity"], G);
	}
	let r = "coordsPx";
	if ("shape-scale" in n) {
		const g = Y(i, n["shape-scale"], ut);
		r = "coordsPx / ".concat(g);
	}
	let o = null;
	if ("shape-fill-color" in n) {
		o = Y(i, n["shape-fill-color"], pe);
	}
	let a = null;
	if ("shape-stroke-color" in n) {
		a = Y(i, n["shape-stroke-color"], pe);
	}
	let l = null;
	if ("shape-stroke-width" in n) {
		l = Y(i, n["shape-stroke-width"], G);
	}
	const h = Y(i, n["shape-points"], G);
	let c = "0.";
	if ("shape-angle" in n) {
		c = Y(i, n["shape-angle"], G);
	}
	let u, d = Y(i, n["shape-radius"], G);
	if ("shape-radius2" in n) {
		let g = Y(i, n["shape-radius2"], G);
		if (l !== null) {
			g = "".concat(g, " + ").concat(l, " * 0.5");
		}
		u = "starDistanceField(".concat(r, ", ").concat(h, ", ").concat(d, ", ").concat(g, ", ").concat(c, ")");
	} else u = "regularDistanceField(".concat(r, ", ").concat(h, ", ").concat(d, ", ").concat(c, ")");
	const f = hf(u, o, a, l, s);
	e.setSymbolColorExpression(f);
}
function uC(n, e, t, i) {
	let s = "vec4(1.0)";
	if ("icon-color" in n) {
		s = Y(i, n["icon-color"], pe);
	}
	if ("icon-opacity" in n) {
		s = "".concat(s, " * vec4(1.0, 1.0, 1.0, ").concat(Y(i, n["icon-opacity"], G), ")");
	}
	const r = to(n["icon-src"]), o = Eh(n, e, t, "icon-", r);
	e.setSymbolColorExpression("".concat(s, " * texture2D(u_texture").concat(r, ", v_texCoord)")).setSymbolSizeExpression(o);
	if ("icon-width" in n && "icon-height" in n) {
		e.setSymbolSizeExpression("vec2(".concat(Y(i, n["icon-width"], G), ", ").concat(Y(i, n["icon-height"], G), ")"));
	}
	if ("icon-offset" in n && "icon-size" in n) {
		const a = Y(i, n["icon-size"], He);
		const l = e.getSymbolSizeExpression();
		e.setSymbolSizeExpression(a);
		const h = Th(n, "icon-", i, "v_quadSizePx", a);
		e.setTextureCoordinateExpression("(vec4((".concat(h, ").xyxy) + vec4(0., 0., ").concat(a, ")) / (").concat(l, ").xyxy"));
	}
	xh(n, e, i, "icon-");
	if ("icon-anchor" in n) {
		const a = Y(i, n["icon-anchor"], He);
		let l = "1.0";
		if ("icon-scale" in n) {
			l = Y(i, n["icon-scale"], ut);
		}
		let h;
		n["icon-anchor-x-units"] === "pixels" && n["icon-anchor-y-units"] === "pixels" ? h = "".concat(a, " * ").concat(l) : n["icon-anchor-x-units"] === "pixels" ? h = "".concat(a, " * vec2(vec2(").concat(l, ").x, v_quadSizePx.y)") : n["icon-anchor-y-units"] === "pixels" ? h = "".concat(a, " * vec2(v_quadSizePx.x, vec2(").concat(l, ").x)") : h = "".concat(a, " * v_quadSizePx");
		let c = "v_quadSizePx * vec2(0.5, -0.5) + ".concat(h, " * vec2(-1., 1.)");
		if ("icon-anchor-origin" in n) switch (n["icon-anchor-origin"]) {
			case "top-right":
				c = "v_quadSizePx * -0.5 + ".concat(h);
				break;
			case "bottom-left":
				c = "v_quadSizePx * 0.5 - ".concat(h);
				break;
			case "bottom-right":
				c = "v_quadSizePx * vec2(-0.5, 0.5) + ".concat(h, " * vec2(1., -1.)");
				break;
		}
		e.setSymbolOffsetExpression("".concat(e.getSymbolOffsetExpression(), " + ").concat(c));
	}
}
function dC(n, e, t, i) {
	if ("stroke-color" in n) {
		e.setStrokeColorExpression(Y(i, n["stroke-color"], pe));
	}
	if ("stroke-pattern-src" in n) {
		const s = to(n["stroke-pattern-src"]);
		const r = Eh(n, e, t, "stroke-pattern-", s);
		let o = r;
		let a = "vec2(0.)";
		if ("stroke-pattern-offset" in n && "stroke-pattern-size" in n) {
			o = Y(i, n["stroke-pattern-size"], He);
			a = Th(n, "stroke-pattern-", i, r, o);
		}
		let l = "0.";
		if ("stroke-pattern-spacing" in n) {
			l = Y(i, n["stroke-pattern-spacing"], G);
		}
		let h = "0.";
		if ("stroke-pattern-start-offset" in n) {
			h = Y(i, n["stroke-pattern-start-offset"], G);
		}
		i.functions.sampleStrokePattern = "vec4 sampleStrokePattern(sampler2D texture, vec2 textureSize, vec2 textureOffset, vec2 sampleSize, float spacingPx, float startOffsetPx, float currentLengthPx, float currentRadiusRatio, float lineWidth) {\n  float currentLengthScaled = (currentLengthPx - startOffsetPx) * sampleSize.y / lineWidth;\n  float spacingScaled = spacingPx * sampleSize.y / lineWidth;\n  float uCoordPx = mod(currentLengthScaled, (sampleSize.x + spacingScaled));\n  float isInsideOfPattern = step(uCoordPx, sampleSize.x);\n  float vCoordPx = (-currentRadiusRatio * 0.5 + 0.5) * sampleSize.y;\n  // make sure that we're not sampling too close to the borders to avoid interpolation with outside pixels\n  uCoordPx = clamp(uCoordPx, 0.5, sampleSize.x - 0.5);\n  vCoordPx = clamp(vCoordPx, 0.5, sampleSize.y - 0.5);\n  vec2 texCoord = (vec2(uCoordPx, vCoordPx) + textureOffset) / textureSize;\n  return texture2D(texture, texCoord) * vec4(1.0, 1.0, 1.0, isInsideOfPattern);\n}";
		const c = "u_texture".concat(s);
		let u = "1.";
		if ("stroke-color" in n) {
			u = e.getStrokeColorExpression();
		}
		e.setStrokeColorExpression("".concat(u, " * sampleStrokePattern(").concat(c, ", ").concat(r, ", ").concat(a, ", ").concat(o, ", ").concat(l, ", ").concat(h, ", currentLengthPx, currentRadiusRatio, v_width)"));
		i.functions.computeStrokePatternLength = "float computeStrokePatternLength(vec2 sampleSize, float spacingPx, float lineWidth) {\n  float patternLengthPx = sampleSize.x / sampleSize.y * lineWidth;\n  return patternLengthPx + spacingPx;\n}";
		e.setStrokePatternLengthExpression("computeStrokePatternLength(".concat(o, ", ").concat(l, ", v_width)"));
	}
	if ("stroke-width" in n) {
		e.setStrokeWidthExpression(Y(i, n["stroke-width"], G));
	}
	if ("stroke-offset" in n) {
		e.setStrokeOffsetExpression(Y(i, n["stroke-offset"], G));
	}
	if ("stroke-line-cap" in n) {
		e.setStrokeCapExpression(Y(i, n["stroke-line-cap"], Ie));
	}
	if ("stroke-line-join" in n) {
		e.setStrokeJoinExpression(Y(i, n["stroke-line-join"], Ie));
	}
	if ("stroke-miter-limit" in n) {
		e.setStrokeMiterLimitExpression(Y(i, n["stroke-miter-limit"], G));
	}
	if ("stroke-line-dash" in n) {
		i.functions.getSingleDashDistance = "float getSingleDashDistance(float distance, float radius, float dashOffset, float dashLength, float dashLengthTotal, float capType, float lineWidth) {\n  float localDistance = mod(distance, dashLengthTotal);\n  float distanceSegment = abs(localDistance - dashOffset - dashLength * 0.5) - dashLength * 0.5;\n  distanceSegment = min(distanceSegment, dashLengthTotal - localDistance);\n  if (capType == ".concat(Ht("square"), ") {\n    distanceSegment -= lineWidth * 0.5;\n  } else if (capType == ").concat(Ht("round"), ") {\n    distanceSegment = min(distanceSegment, sqrt(distanceSegment * distanceSegment + radius * radius) - lineWidth * 0.5);\n  }\n  return distanceSegment;\n}");
		let s = n["stroke-line-dash"].map((g) => Y(i, g, G));
		if (s.length % 2 === 1) {
			s = [...s, ...s];
		}
		let r = "0.";
		if ("stroke-line-dash-offset" in n) {
			r = Y(i, n["stroke-line-dash-offset"], G);
		}
		const o = to(n["stroke-line-dash"]);
		const a = "dashDistanceField_".concat(o);
		const l = s.map((g, m) => "float dashLength".concat(m)).join(", ");
		const h = s.map((g, m) => "dashLength".concat(m)).join(" + ");
		let c = "0.";
		let u = "getSingleDashDistance(distance, radius, ".concat(c, ", dashLength0, totalDashLength, capType, lineWidth)");
		for (let g = 2; g < s.length; g += 2) {
			c = "".concat(c, " + dashLength").concat(g - 2, " + dashLength").concat(g - 1);
			u = "min(".concat(u, ", getSingleDashDistance(distance, radius, ").concat(c, ", dashLength").concat(g, ", totalDashLength, capType, lineWidth))");
		}
		i.functions[a] = "float ".concat(a, "(float distance, float radius, float capType, float lineWidth, ").concat(l, ") {\n  float totalDashLength = ").concat(h, ";\n  return ").concat(u, ";\n}");
		const d = s.map((g, m) => "".concat(g)).join(", ");
		e.setStrokeDistanceFieldExpression("".concat(a, "(currentLengthPx + ").concat(r, ", currentRadiusPx, capType, v_width, ").concat(d, ")"));
		let f = s.join(" + ");
		if (e.getStrokePatternLengthExpression()) {
			i.functions.combinePatternLengths = "float combinePatternLengths(float patternLength1, float patternLength2) {\n  return patternLength1 * patternLength2;\n}";
			f = "combinePatternLengths(".concat(e.getStrokePatternLengthExpression(), ", ").concat(f, ")");
		}
		e.setStrokePatternLengthExpression(f);
	}
}
function fC(n, e, t, i) {
	if ("fill-color" in n) {
		e.setFillColorExpression(Y(i, n["fill-color"], pe));
	}
	if ("fill-pattern-src" in n) {
		const s = to(n["fill-pattern-src"]);
		const r = Eh(n, e, t, "fill-pattern-", s);
		let o = r;
		let a = "vec2(0.)";
		if ("fill-pattern-offset" in n && "fill-pattern-size" in n) {
			o = Y(i, n["fill-pattern-size"], He);
			a = Th(n, "fill-pattern-", i, r, o);
		}
		i.functions.sampleFillPattern = "vec4 sampleFillPattern(sampler2D texture, vec2 textureSize, vec2 textureOffset, vec2 sampleSize, vec2 pxOrigin, vec2 pxPosition) {\n  float scaleRatio = pow(2., mod(u_zoom + 0.5, 1.) - 0.5);\n  vec2 pxRelativePos = pxPosition - pxOrigin;\n  // rotate the relative position from origin by the current view rotation\n  pxRelativePos = vec2(pxRelativePos.x * cos(u_rotation) - pxRelativePos.y * sin(u_rotation), pxRelativePos.x * sin(u_rotation) + pxRelativePos.y * cos(u_rotation));\n  // sample position is computed according to the sample offset & size\n  vec2 samplePos = mod(pxRelativePos / scaleRatio, sampleSize);\n  // also make sure that we're not sampling too close to the borders to avoid interpolation with outside pixels\n  samplePos = clamp(samplePos, vec2(0.5), sampleSize - vec2(0.5));\n  samplePos.y = sampleSize.y - samplePos.y; // invert y axis so that images appear upright\n  return texture2D(texture, (samplePos + textureOffset) / textureSize);\n}";
		const l = "u_texture".concat(s);
		let h = "1.";
		if ("fill-color" in n) {
			h = e.getFillColorExpression();
		}
		e.setFillColorExpression("".concat(h, " * sampleFillPattern(").concat(l, ", ").concat(r, ", ").concat(a, ", ").concat(o, ", pxOrigin, pxPos)"));
	}
}
function Wc(n, e, t) {
	const i = tf(), s = new lf(), r = {};
	"icon-src" in n ? uC(n, s, r, i) : "shape-points" in n ? cC(n, s, r, i) : "circle-radius" in n && hC(n, s, r, i);
	dC(n, s, r, i);
	fC(n, s, r, i);
	if (t) {
		const l = Y(i, t, we);
		s.setFragmentDiscardExpression("!".concat(l));
	}
	const o = {};
	function a(l, h, c, u) {
		if (!i[l]) return;
		const d = Va(c), f = ph(c);
		s.addAttribute("a_".concat(h), d);
		o[h] = {
			size: f,
			callback: u
		};
	}
	a("geometryType", sf, Ie, (l) => Ps($l(l.getGeometry())));
	a("featureId", nf, Ie | G, (l) => {
		var c;
		const h = (c = l.getId()) != null ? c : null;
		return typeof h == "string" ? Ps(h) : h;
	});
	rf(s, i);
	return {
		builder: s,
		attributes: {
			...o,
			...af(i)
		},
		uniforms: {
			...r,
			...of(i, e)
		}
	};
}
const gC = [];
let ma;
function _C() {
	if (!ma) {
		ma = nC();
	}
	return ma;
}
let mC = 0;
const ot = {
	POSITION: "a_position",
	LOCAL_POSITION: "a_localPosition",
	SEGMENT_START: "a_segmentStart",
	SEGMENT_END: "a_segmentEnd",
	MEASURE_START: "a_measureStart",
	MEASURE_END: "a_measureEnd",
	ANGLE_TANGENT_SUM: "a_angleTangentSum",
	JOIN_ANGLES: "a_joinAngles",
	DISTANCE_LOW: "a_distanceLow",
	DISTANCE_HIGH: "a_distanceHigh"
};
class pC {
	constructor(e, t, i, s) {
		this.helper_;
		this.hitDetectionEnabled_ = !!s;
		this.styleShaders = xC(e, t);
		this.customAttributes_ = {};
		this.uniforms_ = {};
		if (this.hitDetectionEnabled_) {
			this.customAttributes_.hitColor = {
				callback() {
					return sC(this.ref, gC);
				},
				size: 2
			};
		}
		for (const r of this.styleShaders) {
			for (const o in r.attributes) o in this.customAttributes_ || (this.customAttributes_[o] = r.attributes[o]);
			for (const o in r.uniforms) o in this.uniforms_ || (this.uniforms_[o] = r.uniforms[o]);
		}
		this.renderPasses_ = this.styleShaders.map((r) => {
			const o = {}, a = Object.entries(this.customAttributes_).map(([l, h]) => ({
				name: l in r.attributes || l === "hitColor" ? "a_".concat(l) : null,
				size: h.size || 1,
				type: Oe.FLOAT
			}));
			if (r.builder.getFillVertexShader()) {
				o.fillRenderPass = {
					vertexShader: r.builder.getFillVertexShader(),
					fragmentShader: r.builder.getFillFragmentShader(),
					attributesDesc: [{
						name: ot.POSITION,
						size: 2,
						type: Oe.FLOAT
					}, ...a],
					instancedAttributesDesc: [],
					instancePrimitiveVertexCount: 3
				};
			}
			if (r.builder.getStrokeVertexShader()) {
				o.strokeRenderPass = {
					vertexShader: r.builder.getStrokeVertexShader(),
					fragmentShader: r.builder.getStrokeFragmentShader(),
					attributesDesc: [{
						name: ot.LOCAL_POSITION,
						size: 2,
						type: Oe.FLOAT
					}],
					instancedAttributesDesc: [
						{
							name: ot.SEGMENT_START,
							size: 2,
							type: Oe.FLOAT
						},
						{
							name: ot.MEASURE_START,
							size: 1,
							type: Oe.FLOAT
						},
						{
							name: ot.SEGMENT_END,
							size: 2,
							type: Oe.FLOAT
						},
						{
							name: ot.MEASURE_END,
							size: 1,
							type: Oe.FLOAT
						},
						{
							name: ot.JOIN_ANGLES,
							size: 2,
							type: Oe.FLOAT
						},
						{
							name: ot.DISTANCE_LOW,
							size: 1,
							type: Oe.FLOAT
						},
						{
							name: ot.DISTANCE_HIGH,
							size: 1,
							type: Oe.FLOAT
						},
						{
							name: ot.ANGLE_TANGENT_SUM,
							size: 1,
							type: Oe.FLOAT
						},
						...a
					],
					instancePrimitiveVertexCount: 6
				};
			}
			if (r.builder.getSymbolVertexShader()) {
				o.symbolRenderPass = {
					vertexShader: r.builder.getSymbolVertexShader(),
					fragmentShader: r.builder.getSymbolFragmentShader(),
					attributesDesc: [{
						name: ot.LOCAL_POSITION,
						size: 2,
						type: Oe.FLOAT
					}],
					instancedAttributesDesc: [{
						name: ot.POSITION,
						size: 2,
						type: Oe.FLOAT
					}, ...a],
					instancePrimitiveVertexCount: 6
				};
			}
			return o;
		});
		this.hasFill_ = this.renderPasses_.some((r) => r.fillRenderPass);
		this.hasStroke_ = this.renderPasses_.some((r) => r.strokeRenderPass);
		this.hasSymbol_ = this.renderPasses_.some((r) => r.symbolRenderPass);
		this.setHelper(i);
	}
	async generateBuffers(e, t) {
		if (e.isEmpty()) return null;
		const i = this.generateRenderInstructions_(e, t), [s, r, o] = await Promise.all([
			this.generateBuffersForType_(i.polygonInstructions, "Polygon", t),
			this.generateBuffersForType_(i.lineStringInstructions, "LineString", t),
			this.generateBuffersForType_(i.pointInstructions, "Point", t)
		]), a = ps(Se(), t);
		return {
			polygonBuffers: s,
			lineStringBuffers: r,
			pointBuffers: o,
			invertVerticesTransform: a
		};
	}
	generateRenderInstructions_(e, t) {
		const i = this.hasFill_ ? lC(e.polygonBatch, new Float32Array(0), this.customAttributes_, t) : null, s = this.hasStroke_ ? aC(e.lineStringBatch, new Float32Array(0), this.customAttributes_, t) : null, r = this.hasSymbol_ ? oC(e.pointBatch, new Float32Array(0), this.customAttributes_, t) : null;
		return {
			polygonInstructions: i,
			lineStringInstructions: s,
			pointInstructions: r
		};
	}
	generateBuffersForType_(e, t, i) {
		const s = mC++;
		let r;
		switch (t) {
			case "Polygon":
				r = _a.GENERATE_POLYGON_BUFFERS;
				break;
			case "LineString":
				r = _a.GENERATE_LINE_STRING_BUFFERS;
				break;
			case "Point":
				r = _a.GENERATE_POINT_BUFFERS;
				break;
		}
		const o = {
			id: s,
			type: r,
			renderInstructions: e.buffer,
			renderInstructionsTransform: i,
			customAttributesSize: Eo(this.customAttributes_)
		}, a = _C();
		a.postMessage(o, [e.buffer]);
		e = null;
		return new Promise((l) => {
			const h = (c) => {
				const u = c.data;
				if (c.data.id !== s || (a.removeEventListener("message", h), !this.helper_.getGL())) return;
				const d = new AT(dh, Sr).fromArrayBuffer(c.data.indicesBuffer), f = new AT(Jr, Sr).fromArrayBuffer(c.data.vertexAttributesBuffer), g = new AT(Jr, Sr).fromArrayBuffer(c.data.instanceAttributesBuffer);
				this.helper_.flushBufferData(d);
				this.helper_.flushBufferData(f);
				this.helper_.flushBufferData(g);
				l([
					d,
					f,
					g
				]);
			};
			a.addEventListener("message", h);
		});
	}
	render(e, t, i) {
		for (const s of this.renderPasses_) {
			if (s.fillRenderPass) {
				this.renderInternal_(e.polygonBuffers[0], e.polygonBuffers[1], e.polygonBuffers[2], s.fillRenderPass, t, i);
			}
			if (s.strokeRenderPass) {
				this.renderInternal_(e.lineStringBuffers[0], e.lineStringBuffers[1], e.lineStringBuffers[2], s.strokeRenderPass, t, i);
			}
			if (s.symbolRenderPass) {
				this.renderInternal_(e.pointBuffers[0], e.pointBuffers[1], e.pointBuffers[2], s.symbolRenderPass, t, i);
			}
		}
	}
	renderInternal_(e, t, i, s, r, o) {
		const a = e.getSize();
		if (a === 0) return;
		const l = s.instancedAttributesDesc.length;
		this.helper_.useProgram(s.program, r);
		this.helper_.bindBuffer(t);
		this.helper_.bindBuffer(e);
		this.helper_.enableAttributes(s.attributesDesc);
		this.helper_.bindBuffer(i);
		this.helper_.enableAttributesInstanced(s.instancedAttributesDesc);
		o();
		if (s.instancedAttributesDesc.length) {
			const h = s.instancedAttributesDesc.reduce((u, d) => u + (d.size || 1), 0);
			const c = i.getSize() / h;
			this.helper_.drawElementsInstanced(0, a, c);
		} else this.helper_.drawElements(0, a);
	}
	setHelper(e, t = null) {
		this.helper_ = e;
		for (const i of this.renderPasses_) {
			if (i.fillRenderPass) {
				i.fillRenderPass.program = this.helper_.getProgram(i.fillRenderPass.fragmentShader, i.fillRenderPass.vertexShader);
			}
			if (i.strokeRenderPass) {
				i.strokeRenderPass.program = this.helper_.getProgram(i.strokeRenderPass.fragmentShader, i.strokeRenderPass.vertexShader);
			}
			if (i.symbolRenderPass) {
				i.symbolRenderPass.program = this.helper_.getProgram(i.symbolRenderPass.fragmentShader, i.symbolRenderPass.vertexShader);
			}
		}
		this.helper_.addUniforms(this.uniforms_);
		if (t) {
			if (t.polygonBuffers) {
				this.helper_.flushBufferData(t.polygonBuffers[0]);
				this.helper_.flushBufferData(t.polygonBuffers[1]);
				this.helper_.flushBufferData(t.polygonBuffers[2]);
			}
			if (t.lineStringBuffers) {
				this.helper_.flushBufferData(t.lineStringBuffers[0]);
				this.helper_.flushBufferData(t.lineStringBuffers[1]);
				this.helper_.flushBufferData(t.lineStringBuffers[2]);
			}
			if (t.pointBuffers) {
				this.helper_.flushBufferData(t.pointBuffers[0]);
				this.helper_.flushBufferData(t.pointBuffers[1]);
				this.helper_.flushBufferData(t.pointBuffers[2]);
			}
		}
	}
}
function xC(n, e) {
	const t = Array.isArray(n) ? n : [n];
	if ("style" in t[0]) {
		const i = [];
		const s = t;
		const r = [];
		for (const o of t) {
			const a = Array.isArray(o.style) ? o.style : [o.style];
			let l = o.filter;
			if (o.else && r.length) {
				l = ["all", ...r.map((c) => ["!", c])];
				if (o.filter) {
					l.push(o.filter);
				}
				if (l.length < 3) {
					l = l[1];
				}
			}
			if (o.filter) {
				r.push(o.filter);
			}
			const h = a.map((c) => Wc(c, e, l));
			i.push(...h);
		}
		return i;
	}
	return "builder" in t[0] ? t : t.map((i) => Wc(i, e, null));
}
const wt = new Uint8Array(4);
class EC {
	constructor(e, t) {
		this.helper_ = e;
		const i = e.getGL();
		this.texture_ = i.createTexture();
		this.framebuffer_ = i.createFramebuffer();
		this.depthbuffer_ = i.createRenderbuffer();
		this.size_ = t || [1, 1];
		this.data_ = new Uint8Array(0);
		this.dataCacheDirty_ = true;
		this.updateSize_();
	}
	setSize(e) {
		if (!kt(e, this.size_)) {
			this.size_[0] = e[0];
			this.size_[1] = e[1];
			this.updateSize_();
		}
	}
	getSize() {
		return this.size_;
	}
	clearCachedData() {
		this.dataCacheDirty_ = true;
	}
	readAll() {
		if (this.dataCacheDirty_) {
			const e = this.size_;
			const t = this.helper_.getGL();
			t.bindFramebuffer(t.FRAMEBUFFER, this.framebuffer_);
			t.readPixels(0, 0, this.size_[0], this.size_[1], t.RGBA, t.UNSIGNED_BYTE, this.data_);
			this.dataCacheDirty_ = false;
		}
		return this.data_;
	}
	readPixel(e, t) {
		if (e < 0 || t < 0 || e > this.size_[0] || t >= this.size_[1]) return wt[0] = 0, wt[1] = 0, wt[2] = 0, wt[3] = 0, wt;
		this.readAll();
		const i = Math.floor(e) + (this.size_[1] - Math.floor(t) - 1) * this.size_[0];
		wt[0] = this.data_[i * 4];
		wt[1] = this.data_[i * 4 + 1];
		wt[2] = this.data_[i * 4 + 2];
		wt[3] = this.data_[i * 4 + 3];
		return wt;
	}
	getTexture() {
		return this.texture_;
	}
	getFramebuffer() {
		return this.framebuffer_;
	}
	getDepthbuffer() {
		return this.depthbuffer_;
	}
	updateSize_() {
		const e = this.size_, t = this.helper_.getGL();
		this.texture_ = this.helper_.createTexture(this.size_, null, this.texture_);
		t.bindFramebuffer(t.FRAMEBUFFER, this.framebuffer_);
		t.viewport(0, 0, this.size_[0], this.size_[1]);
		t.framebufferTexture2D(t.FRAMEBUFFER, t.COLOR_ATTACHMENT0, t.TEXTURE_2D, this.texture_, 0);
		t.bindRenderbuffer(t.RENDERBUFFER, this.depthbuffer_);
		t.renderbufferStorage(t.RENDERBUFFER, t.DEPTH_COMPONENT16, this.size_[0], this.size_[1]);
		t.framebufferRenderbuffer(t.FRAMEBUFFER, t.DEPTH_ATTACHMENT, t.RENDERBUFFER, this.depthbuffer_);
		this.data_ = new Uint8Array(this.size_[0] * this.size_[1] * 4);
	}
}
function CC(n, e) {
	const t = n.viewState.projection, s = e.getSource().getWrapX() && n.viewState.projection.canWrapX(), r = n.viewState.projection.getExtent(), o = n.extent, a = s ? J(r) : null, l = s ? Math.ceil((n.extent[2] - r[2]) / a) + 1 : 1;
	return [
		s ? Math.floor((n.extent[0] - r[0]) / a) : 0,
		l,
		a
	];
}
const hn = {
	...Vt,
	RENDER_EXTENT: "u_renderExtent",
	PATTERN_ORIGIN: "u_patternOrigin",
	GLOBAL_ALPHA: "u_globalAlpha"
};
class RC extends fh {
	constructor(e, t) {
		const i = {
			[hn.RENDER_EXTENT]: [
				0,
				0,
				0,
				0
			],
			[hn.PATTERN_ORIGIN]: [0, 0],
			[hn.GLOBAL_ALPHA]: 1
		};
		super(e, {
			uniforms: i,
			postProcesses: t.postProcesses
		});
		this.hitDetectionEnabled_ = !t.disableHitDetection;
		this.hitRenderTarget_;
		this.sourceRevision_ = -1;
		this.previousExtent_ = je();
		this.currentTransform_ = Se();
		this.tmpCoords_ = [0, 0];
		this.tmpTransform_ = Se();
		this.tmpMat4_ = Hd();
		this.currentFrameStateTransform_ = Se();
		this.styleVariables_ = {};
		this.style_ = [];
		this.styleRenderer_ = null;
		this.buffers_ = null;
		this.applyOptions_(t);
		this.batch_ = new eo();
		this.initialFeaturesAdded_ = false;
		this.sourceListenKeys_ = null;
	}
	addInitialFeatures_(e) {
		const t = this.getLayer().getSource();
		let i;
		this.batch_.addFeatures(t.getFeatures(), i);
		this.sourceListenKeys_ = [
			Z(t, Te.ADDFEATURE, this.handleSourceFeatureAdded_.bind(this, i)),
			Z(t, Te.CHANGEFEATURE, this.handleSourceFeatureChanged_.bind(this, i), this),
			Z(t, Te.REMOVEFEATURE, this.handleSourceFeatureDelete_, this),
			Z(t, Te.CLEAR, this.handleSourceFeatureClear_, this)
		];
	}
	applyOptions_(e) {
		this.styleVariables_ = e.variables;
		this.style_ = e.style;
	}
	createRenderers_() {
		this.buffers_ = null;
		this.styleRenderer_ = new pC(this.style_, this.styleVariables_, this.helper, this.hitDetectionEnabled_);
	}
	reset(e) {
		this.applyOptions_(e);
		if (this.helper) {
			this.createRenderers_();
		}
		super.reset(e);
	}
	afterHelperCreated() {
		this.styleRenderer_ ? this.styleRenderer_.setHelper(this.helper, this.buffers_) : this.createRenderers_();
		if (this.hitDetectionEnabled_) {
			this.hitRenderTarget_ = new EC(this.helper);
		}
	}
	handleSourceFeatureAdded_(e, t) {
		const i = t.feature;
		this.batch_.addFeature(t.feature, e);
	}
	handleSourceFeatureChanged_(e, t) {
		const i = t.feature;
		this.batch_.changeFeature(t.feature, e);
	}
	handleSourceFeatureDelete_(e) {
		const t = e.feature;
		this.batch_.removeFeature(e.feature);
	}
	handleSourceFeatureClear_() {
		this.batch_.clear();
	}
	applyUniforms_(e) {
		_u(this.tmpTransform_, this.currentFrameStateTransform_);
		co(this.tmpTransform_, e);
		this.helper.setUniformMatrixValue(hn.PROJECTION_MATRIX, za(this.tmpMat4_, this.tmpTransform_));
		ps(this.tmpTransform_, this.tmpTransform_);
		this.helper.setUniformMatrixValue(hn.SCREEN_TO_WORLD_MATRIX, za(this.tmpMat4_, this.tmpTransform_));
		this.tmpCoords_[0] = 0;
		this.tmpCoords_[1] = 0;
		ps(this.tmpTransform_, e);
		xe(this.tmpTransform_, this.tmpCoords_);
		this.helper.setUniformFloatVec2(hn.PATTERN_ORIGIN, this.tmpCoords_);
	}
	renderFrame(e) {
		const t = this.helper.getGL();
		this.preRender(t, e);
		const [i, s, r] = CC(e, this.getLayer());
		this.helper.prepareDraw(e);
		this.renderWorlds(e, false, i, s, r);
		this.helper.finalizeDraw(e, this.dispatchPreComposeEvent, this.dispatchPostComposeEvent);
		const o = this.helper.getCanvas();
		if (this.hitDetectionEnabled_) {
			this.renderWorlds(e, true, i, s, r);
			this.hitRenderTarget_.clearCachedData();
		}
		this.postRender(t, e);
		return o;
	}
	prepareFrameInternal(e) {
		if (!this.initialFeaturesAdded_) {
			this.addInitialFeatures_(e);
			this.initialFeaturesAdded_ = true;
		}
		const t = this.getLayer(), i = t.getSource(), s = e.viewState, r = !e.viewHints[de.ANIMATING] && !e.viewHints[de.INTERACTING], o = !Si(this.previousExtent_, e.extent), a = this.sourceRevision_ < i.getRevision();
		if (a) {
			this.sourceRevision_ = i.getRevision();
		}
		if (r && (o || a)) {
			const l = s.projection;
			const h = s.resolution;
			const c = t instanceof Pp ? t.getRenderBuffer() : 0;
			const u = tt(e.extent, c * s.resolution);
			i.loadFeatures(u, s.resolution, s.projection);
			this.ready = false;
			const d = this.helper.makeProjectionTransform(e, Se());
			this.styleRenderer_.generateBuffers(this.batch_, d).then((f) => {
				if (this.buffers_) {
					this.disposeBuffers(this.buffers_);
				}
				this.buffers_ = f;
				this.ready = true;
				this.getLayer().changed();
			});
			this.previousExtent_ = e.extent.slice();
		}
		return true;
	}
	renderWorlds(e, t, i, s, r) {
		let o = i;
		if (t) {
			this.hitRenderTarget_.setSize([Math.floor(e.size[0] / 2), Math.floor(e.size[1] / 2)]);
			this.helper.prepareDrawToRenderTarget(e, this.hitRenderTarget_, true);
		}
		do {
			this.helper.makeProjectionTransform(e, this.currentFrameStateTransform_);
			mu(this.currentFrameStateTransform_, o * r, 0);
			if (this.buffers_) {
				this.styleRenderer_.render(this.buffers_, e, () => {
					this.applyUniforms_(this.buffers_.invertVerticesTransform);
					this.helper.applyHitDetectionUniform(t);
				});
			}
		} while (++o < s);
	}
	forEachFeatureAtCoordinate(e, t, i, s, r) {
		ee(this.hitDetectionEnabled_, "`forEachFeatureAtCoordinate` cannot be used on a WebGL layer if the hit detection logic has been disabled using the `disableHitDetection: true` option.");
		if (!this.styleRenderer_ || !this.hitDetectionEnabled_) return;
		const o = xe(t.coordinateToPixelTransform, e.slice()), a = this.hitRenderTarget_.readPixel(o[0] / 2, o[1] / 2), l = [
			a[0] / 255,
			a[1] / 255,
			a[2] / 255,
			a[3] / 255
		], h = rC(l), c = this.batch_.getFeatureFromRef(h);
		if (c) return s(c, this.getLayer(), null);
	}
	disposeBuffers(e) {
		const t = (i) => {
			for (const s of i) s && this.helper.deleteBuffer(s);
		};
		if (e.pointBuffers) {
			t(e.pointBuffers);
		}
		if (e.lineStringBuffers) {
			t(e.lineStringBuffers);
		}
		if (e.polygonBuffers) {
			t(e.polygonBuffers);
		}
	}
	disposeInternal() {
		if (this.buffers_) {
			this.disposeBuffers(this.buffers_);
		}
		if (this.sourceListenKeys_) {
			this.sourceListenKeys_.forEach(function(e) {
				se(e);
			});
			this.sourceListenKeys_ = null;
		}
		super.disposeInternal();
	}
	renderDeclutter() {}
}
const Wt = {
	BLUR: "blur",
	GRADIENT: "gradient",
	RADIUS: "radius"
};
const vC = [
	"#00f",
	"#0ff",
	"#0f0",
	"#ff0",
	"#f00"
];
class wC extends Pp {
	constructor(e) {
		var s;
		e = e || {};
		const t = Object.assign({}, e);
		delete t.gradient;
		delete t.radius;
		delete t.blur;
		delete t.weight;
		super(t);
		this.on;
		this.once;
		this.un;
		this.filter_ = (s = e.filter) != null ? s : true;
		this.styleVariables_ = e.variables || {};
		this.gradient_ = null;
		this.addChangeListener(Wt.GRADIENT, this.handleGradientChanged_);
		this.setGradient(e.gradient ? e.gradient : vC);
		this.setBlur(e.blur !== "undefined" ? e.blur : 15);
		this.setRadius(e.radius !== "undefined" ? e.radius : 8);
		const i = e.weight ? e.weight : "weight";
		this.weight_ = i;
		this.setRenderOrder(null);
	}
	getBlur() {
		return this.get(Wt.BLUR);
	}
	getGradient() {
		return this.get(Wt.GRADIENT);
	}
	getRadius() {
		return this.get(Wt.RADIUS);
	}
	handleGradientChanged_() {
		this.gradient_ = PC(this.getGradient());
	}
	setBlur(e) {
		const t = this.get(Wt.BLUR);
		this.set(Wt.BLUR, e);
		if (typeof e == "number" && typeof t == "number") {
			this.changed();
			return;
		}
		this.clearRenderer();
	}
	setGradient(e) {
		this.set(Wt.GRADIENT, e);
	}
	setRadius(e) {
		const t = this.get(Wt.RADIUS);
		this.set(Wt.RADIUS, e);
		if (typeof e == "number" && typeof t == "number") {
			this.changed();
			return;
		}
		this.clearRenderer();
	}
	setFilter(e) {
		this.filter_ = e;
		this.changed();
		this.clearRenderer();
	}
	setWeight(e) {
		this.weight_ = e;
		this.changed();
		this.clearRenderer();
	}
	createRenderer() {
		const e = new lf(), t = tf(), i = Y(t, this.filter_, we);
		let s = Y(t, this.getRadius(), G), r = Y(t, this.getBlur(), G);
		const o = {};
		if (typeof this.getBlur() == "number") {
			r = "a_blur";
			o.a_blur = () => this.getBlur();
			e.addUniform("a_blur", "float");
		}
		if (typeof this.getRadius() == "number") {
			s = "a_radius";
			o.a_radius = () => this.getRadius();
			e.addUniform("a_radius", "float");
		}
		const a = {};
		let l = null;
		if (typeof this.weight_ == "string" || typeof this.weight_ == "function") {
			const u = typeof this.weight_ == "string" ? (d) => d.get(this.weight_) : this.weight_;
			a.prop_weight = {
				size: 1,
				callback: (d) => {
					const f = u(d);
					return f !== "undefined" ? fe(f, 0, 1) : 1;
				}
			};
			l = "a_prop_weight";
			e.addAttribute("a_prop_weight", "float");
		} else {
			const u = [
				"clamp",
				this.weight_,
				0,
				1
			];
			l = Y(t, u, G);
		}
		e.addFragmentShaderFunction("float getBlurSlope() {\n  float blur = max(1., ".concat(r, ");\n  float radius = ").concat(s, ";\n  return radius / blur;\n}")).setSymbolSizeExpression("vec2(".concat(s, " + ").concat(r, ") * 2.")).setSymbolColorExpression("vec4(smoothstep(0., 1., (1. - length(coordsPx * 2. / v_quadSizePx)) * getBlurSlope()) * ".concat(l, ")")).setStrokeColorExpression("vec4(smoothstep(0., 1., (1. - length(currentRadiusPx * 2. / v_width)) * getBlurSlope()) * ".concat(l, ")")).setStrokeWidthExpression("(".concat(s, " + ").concat(r, ") * 2.")).setFillColorExpression("vec4(".concat(l, ")")).setFragmentDiscardExpression("!".concat(i));
		rf(e, t);
		const h = af(t), c = of(t, this.styleVariables_);
		return new RC(this, {
			className: this.getClassName(),
			variables: this.styleVariables_,
			style: {
				builder: e,
				attributes: {
					...h,
					...a
				},
				uniforms: {
					...c,
					...o
				}
			},
			disableHitDetection: false,
			postProcesses: [{
				fragmentShader: "\n            precision mediump float;\n\n            uniform sampler2D u_image;\n            uniform sampler2D u_gradientTexture;\n            uniform float u_opacity;\n\n            varying vec2 v_texCoord;\n\n            void main() {\n              vec4 color = texture2D(u_image, v_texCoord);\n              gl_FragColor.a = color.a * u_opacity;\n              gl_FragColor.rgb = texture2D(u_gradientTexture, vec2(0.5, color.a)).rgb;\n              gl_FragColor.rgb *= gl_FragColor.a;\n            }",
				uniforms: {
					u_gradientTexture: () => this.gradient_,
					u_opacity: () => this.getOpacity()
				}
			}]
		});
	}
	updateStyleVariables(e) {
		Object.assign(this.styleVariables_, e);
		this.changed();
	}
	renderDeclutter() {}
}
function PC(n) {
	const i = Re(1, 256), s = i.createLinearGradient(0, 0, 1, 256), r = 1 / (n.length - 1);
	for (let o = 0, a = n.length; o < a; ++o) s.addColorStop(o * r, n[o]);
	i.fillStyle = s;
	i.fillRect(0, 0, 1, 256);
	return i.canvas;
}
const IC = {
	image: [
		"Polygon",
		"Circle",
		"LineString",
		"Image",
		"Text"
	],
	hybrid: ["Polygon", "LineString"],
	vector: []
};
const Vc = {
	hybrid: [
		"Image",
		"Text",
		"Default"
	],
	vector: [
		"Polygon",
		"Circle",
		"LineString",
		"Image",
		"Text",
		"Default"
	]
};
class FC extends wE {
	constructor(e, t) {
		super(e, t);
		this.boundHandleStyleImageChange_ = this.handleStyleImageChange_.bind(this);
		this.renderedLayerRevision_;
		this.renderedPixelToCoordinateTransform_ = null;
		this.renderedRotation_;
		this.renderedOpacity_ = 1;
		this.tmpTransform_ = Se();
		this.tileClipContexts_ = null;
	}
	enqueueTilesForNextExtent() {
		return this.getLayer().getRenderMode() !== "vector";
	}
	drawTile(e, t, i, s, r, o, a, l) {
		this.updateExecutorGroup_(e, t.pixelRatio, t.viewState.projection);
		if (this.tileImageNeedsRender_(e)) {
			this.renderTileImage_(e, t);
		}
		super.drawTile(e, t, i, s, r, o, a, l);
	}
	getTile(e, t, i, s) {
		const r = this.getOrCreateTile(e, t, i, s);
		if (!r) return null;
		const o = s.viewState, a = s.viewState.resolution, l = s.viewHints, h = this.getLayer().getSource(), c = h.getTileGridForProjection(s.viewState.projection), u = !(s.viewHints[de.ANIMATING] || s.viewHints[de.INTERACTING]), d = c.getZForResolution(s.viewState.resolution, h.zDirection) === e;
		u && d ? r.wantedResolution = s.viewState.resolution : r.wantedResolution || (r.wantedResolution = c.getResolution(e));
		return r;
	}
	prepareFrame(e) {
		const t = this.getLayer().getRevision();
		if (this.renderedLayerRevision_ !== t) {
			this.renderedLayerRevision_ = t;
			this.renderedTiles.length = 0;
		}
		return super.prepareFrame(e);
	}
	updateExecutorGroup_(e, t, i) {
		const s = this.getLayer(), r = s.getRevision(), o = s.getRenderOrder() || null, a = e.wantedResolution, l = e.getReplayState(s);
		if (l.renderedResolution === e.wantedResolution && l.renderedRevision == r && l.renderedPixelRatio === t && l.renderedRenderOrder == o) return;
		const h = s.getSource(), c = !!s.getDeclutter(), u = h.getTileGrid(), f = h.getTileGridForProjection(i).getTileCoordExtent(e.wrappedTileCoord), g = h.getSourceTiles(t, i, e), m = O(s);
		delete e.hitDetectionImageData[m];
		e.executorGroups[m] = [];
		l.dirty = false;
		for (let _ = 0, p = g.length; _ < p; ++_) {
			const y = g[_];
			if (g[_].getState() != M.LOADED) continue;
			const E = h.getProjection();
			const x = g[_].tileCoord;
			let T = u.getTileCoordExtent(g[_].tileCoord);
			if (i && E && !Ae(i, E)) {
				T = as(T, E, i, 32);
			}
			const v = Et(f, T);
			const P = tt(v, s.getRenderBuffer() * a, this.tempExtent);
			const S = Si(T, v) ? null : P;
			const R = new fm(0, v, a, t);
			const I = Aa(a, t);
			const N = function(b, D) {
				let k;
				const B = b.getStyleFunction() || s.getStyleFunction();
				if (B) {
					k = B(b, a);
				}
				if (k) {
					const q = this.renderFeature(b, I, k, R, c, D);
					l.dirty = l.dirty || q;
				}
			};
			const L = g[_].getFeatures();
			if (o && o !== l.renderedRenderOrder) {
				L.sort(o);
			}
			for (let b = 0, D = L.length; b < D; ++b) {
				let k = L[b];
				if (i && y.projection && !Ae(i, y.projection)) {
					k = k.clone();
					k.getGeometry().applyTransform(ji(y.projection, i));
				}
				if (!S || me(S, k.getGeometry().getExtent())) {
					N(k, b);
				}
			}
			const A = R.finish();
			const W = s.getRenderMode() !== "vector" && c && g.length === 1 ? null : v;
			const w = new Cm(W, a, t, h.getOverlaps(), A, s.getRenderBuffer(), true);
			e.executorGroups[m].push(w);
		}
		l.renderedRevision = r;
		l.renderedPixelRatio = t;
		l.renderedRenderOrder = o;
		l.renderedResolution = e.wantedResolution;
	}
	forEachFeatureAtCoordinate(e, t, i, s, r) {
		var E, x;
		const o = t.viewState.resolution, a = t.viewState.rotation;
		i = i == null ? 0 : i;
		const l = this.getLayer(), c = l.getSource().getTileGridForProjection(t.viewState.projection), u = ye([e]);
		tt(u, t.viewState.resolution * i, u);
		const d = {}, f = function(T, v, P) {
			let S = T.getId();
			if (S === "undefined") {
				S = O(T);
			}
			const R = d[S];
			if (d[S]) {
				if (R !== true && P < R.distanceSq) {
					R.geometry = v;
					R.distanceSq = P;
				}
			} else {
				r.push(d[S] = {
					feature: T,
					layer: l,
					geometry: v,
					distanceSq: P,
					callback: s
				});
			}
		}, g = this.renderedTiles, m = O(l), _ = l.getDeclutter(), p = _ ? (x = (E = t.declutter) == null ? "undefined" : E[_]) == null ? "undefined" : x.all().map((T) => T.value) : null;
		let y;
		e: for (let T = 0, v = this.renderedTiles.length; T < v; ++T) {
			const P = g[T];
			const S = c.getTileCoordExtent(g[T].wrappedTileCoord);
			if (!me(S, u)) continue;
			const R = g[T].executorGroups[m];
			for (let I = 0, N = g[T].executorGroups[m].length; I < N; ++I) if (y = g[T].executorGroups[m][I].forEachFeatureAtCoordinate(e, o, a, i, f, p), y) break e;
		}
		return y;
	}
	getFeatures(e) {
		return this.renderedTiles.length === 0 ? Promise.resolve([]) : new Promise((t, i) => {
			const s = this.getLayer(), r = s.getSource(), o = this.renderedProjection, a = this.renderedProjection.getExtent(), l = this.renderedResolution, h = r.getTileGridForProjection(this.renderedProjection), c = xe(this.renderedPixelToCoordinateTransform_, e.slice()), u = h.getTileCoordForCoordAndResolution(c, this.renderedResolution).toString(), d = this.renderedTiles.find((E) => );
			if (!d || d.loadingSourceTiles > 0) {
				t([]);
				return;
			}
			if (r.getWrapX() && this.renderedProjection.canWrapX() && !at(a, h.getTileCoordExtent(d.tileCoord))) {
				Ja(c, this.renderedProjection);
			}
			const f = O(s), g = h.getTileCoordExtent(d.wrappedTileCoord), m = ri(g), _ = [(c[0] - m[0]) / this.renderedResolution, (m[1] - c[1]) / this.renderedResolution], p = d.getSourceTiles().reduce((E, x) => E.concat(x.getFeatures()), []);
			let y = d.hitDetectionImageData[f];
			if (!y) {
				const E = Ne(h.getTileSize(h.getZForResolution(l, r.zDirection)));
				const x = this.renderedRotation_;
				const T = [this.getRenderTransform(h.getTileCoordCenter(d.wrappedTileCoord), l, 0, et, E[0] * et, E[1] * et, 0)];
				y = ed(E, T, p, s.getStyleFunction(), h.getTileCoordExtent(d.wrappedTileCoord), d.getReplayState(s).renderedResolution, this.renderedRotation_);
				d.hitDetectionImageData[f] = y;
			}
			t(td(_, p, y));
		});
	}
	getFeaturesInExtent(e) {
		const t = [], i = this.getTileCache();
		if (i.getCount() === 0) return t;
		const r = this.getLayer().getSource().getTileGridForProjection(this.frameState.viewState.projection), o = r.getZForResolution(this.renderedResolution), a = {};
		i.forEach((l) => {
			if (l.tileCoord[0] !== o || l.getState() !== M.LOADED) return;
			const h = l.getSourceTiles();
			for (let c = 0, u = h.length; c < u; ++c) {
				const d = h[c];
				const f = h[c].getKey();
				if (f in a) continue;
				a[f] = true;
				const g = h[c].tileCoord;
				if (me(e, r.getTileCoordExtent(h[c].tileCoord))) {
					const m = d.getFeatures();
					if (m) for (let _ = 0, p = m.length; _ < p; ++_) {
						const y = m[_];
						const E = m[_].getGeometry();
						if (me(e, E.getExtent())) {
							t.push(m[_]);
						}
					}
				}
			}
		});
		return t;
	}
	handleFontsChanged() {
		const e = this.getLayer();
		if (e.getVisible() && this.renderedLayerRevision_ !== "undefined") {
			e.changed();
		}
	}
	handleStyleImageChange_(e) {
		this.renderIfReadyAndVisible();
	}
	renderDeclutter(e, t) {
		var d;
		const i = this.context, s = this.context.globalAlpha;
		this.context.globalAlpha = t.opacity;
		const r = e.viewHints, o = !(e.viewHints[de.ANIMATING] || e.viewHints[de.INTERACTING]), a = [this.context.canvas.width, this.context.canvas.height], l = this.getLayer().getDeclutter(), h = l ? (d = e.declutter) == null ? "undefined" : d[l] : "undefined", c = O(this.getLayer()), u = this.renderedTiles;
		for (let f = 0, g = this.renderedTiles.length; f < g; ++f) {
			const m = u[f];
			const _ = u[f].executorGroups[c];
			if (u[f].executorGroups[c]) for (let p = u[f].executorGroups[c].length - 1; p >= 0; --p) u[f].executorGroups[c][p].execute(this.context, a, this.getTileRenderTransform(u[f], e), e.viewState.rotation, o, Xr, h);
		}
		this.context.globalAlpha = this.context.globalAlpha;
	}
	renderDeferredInternal(e) {
		const t = this.renderedTiles, i = O(this.getLayer()), s = this.renderedTiles.reduce((l, h, c) => h.executorGroups[i].forEach((u) => l.push({
			executorGroup: u,
			index: c
		})), []), r = s.map(({ executorGroup: l }) => l.getDeferredZIndexContexts()), o = {};
		for (let l = 0, h = s.length; l < h; ++l) {
			const c = s[l].executorGroup.getDeferredZIndexContexts();
			for (const u in c) o[u] = true;
		}
		Object.keys(o).map(Number).sort(Lt).forEach((l) => {
			r.forEach((h, c) => {
				if (h[l]) {
					h[l].forEach((u) => {
						const { executorGroup: d, index: f } = s[c], g = d.getRenderedContext(), m = g.globalAlpha;
						g.globalAlpha = this.renderedOpacity_;
						const _ = this.tileClipContexts_[f];
						if (this.tileClipContexts_[f]) {
							this.tileClipContexts_[f].draw(g);
						}
						u.draw(g);
						if (this.tileClipContexts_[f]) {
							g.restore();
						}
						g.globalAlpha = g.globalAlpha;
						u.clear();
					});
					h[l].length = 0;
				}
			});
		});
	}
	getTileRenderTransform(e, t) {
		const i = t.pixelRatio, s = t.viewState, r = t.viewState.center, o = t.viewState.resolution, a = t.viewState.rotation, l = t.size, h = Math.round(t.size[0] * t.pixelRatio), c = Math.round(t.size[1] * t.pixelRatio), d = this.getLayer().getSource().getTileGridForProjection(t.viewState.projection), f = e.tileCoord, g = d.getTileCoordExtent(e.wrappedTileCoord), m = d.getTileCoordExtent(e.tileCoord, this.tempExtent)[0] - g[0];
		return co(Er(this.inversePixelTransform.slice(), 1 / t.pixelRatio, 1 / t.pixelRatio), this.getRenderTransform(t.viewState.center, t.viewState.resolution, t.viewState.rotation, t.pixelRatio, h, c, m));
	}
	postRender(e, t) {
		var T;
		const i = t.viewHints, s = !(t.viewHints[de.ANIMATING] || t.viewHints[de.INTERACTING]);
		this.renderedPixelToCoordinateTransform_ = t.pixelToCoordinateTransform.slice();
		this.renderedRotation_ = t.viewState.rotation;
		this.renderedOpacity_ = t.layerStatesArray[t.layerIndex].opacity;
		const r = this.getLayer(), o = r.getRenderMode(), a = e.globalAlpha;
		e.globalAlpha = this.renderedOpacity_;
		const l = r.getDeclutter(), h = l ? Vc[o].filter((v) => !Xr.includes(v)) : Vc[o], c = t.viewState, u = t.viewState.rotation, d = r.getSource(), g = d.getTileGridForProjection(t.viewState.projection).getZForResolution(t.viewState.resolution, d.zDirection), m = this.renderedTiles, _ = [], p = [], y = [], E = O(r);
		let x = true;
		for (let v = this.renderedTiles.length - 1; v >= 0; --v) {
			const P = m[v];
			x = x && !m[v].getReplayState(r).dirty;
			const S = m[v].executorGroups[E].filter((w) => w.hasExecutors(h));
			if (S.length === 0) continue;
			const R = this.getTileRenderTransform(m[v], t);
			const I = m[v].tileCoord[0];
			let N = false;
			const L = S[0].getClipCoords(R);
			let A = e;
			let W;
			if (L) {
				W = new _m();
				A = W.getContext();
				for (let w = 0, b = _.length; w < b; ++w) if (g !== I && I < p[w]) {
					const D = _[w];
					if (me([
						L[0],
						L[3],
						L[4],
						L[7]
					], [
						_[w][0],
						_[w][3],
						_[w][4],
						_[w][7]
					])) {
						A.save();
						N = true;
						A.beginPath();
						A.moveTo(L[0], L[1]);
						A.lineTo(L[2], L[3]);
						A.lineTo(L[4], L[5]);
						A.lineTo(L[6], L[7]);
						A.moveTo(_[w][6], _[w][7]);
						A.lineTo(_[w][4], _[w][5]);
						A.lineTo(_[w][2], _[w][3]);
						A.lineTo(_[w][0], _[w][1]);
						A.clip();
					}
				}
				_.push(L);
				p.push(I);
			}
			for (let w = 0, b = S.length; w < b; ++w) S[w].execute(e, [e.canvas.width, e.canvas.height], R, u, s, h, (T = t.declutter) == null ? "undefined" : T[l]);
		}
		e.globalAlpha = e.globalAlpha;
		this.ready = x;
		this.tileClipContexts_ = y;
		if (!t.declutter) {
			this.renderDeferredInternal(t);
		}
		super.postRender(e, t);
	}
	renderFeature(e, t, i, s, r, o) {
		if (!i) return false;
		let a = false;
		if (Array.isArray(i)) for (let l = 0, h = i.length; l < h; ++l) a = jr(s, e, i[l], t, this.boundHandleStyleImageChange_, "undefined", r, o) || a;
		else a = jr(s, e, i, t, this.boundHandleStyleImageChange_, "undefined", r, o);
		return a;
	}
	tileImageNeedsRender_(e) {
		const t = this.getLayer();
		if (t.getRenderMode() === "vector") return false;
		const i = e.getReplayState(t), s = t.getRevision(), r = e.wantedResolution;
		return i.renderedTileResolution !== e.wantedResolution || i.renderedTileRevision !== s;
	}
	renderTileImage_(e, t) {
		const i = this.getLayer(), s = e.getReplayState(i), r = i.getRevision(), o = e.executorGroups[O(i)];
		s.renderedTileRevision = r;
		const a = e.wrappedTileCoord, l = e.wrappedTileCoord[0], h = i.getSource();
		let c = t.pixelRatio;
		const d = t.viewState.projection, f = h.getTileGridForProjection(t.viewState.projection), g = f.getResolution(e.tileCoord[0]), m = t.pixelRatio / e.wantedResolution * g, _ = f.getResolution(e.wrappedTileCoord[0]), p = e.getContext();
		c = Math.round(Math.max(c, m / c));
		const y = h.getTilePixelSize(e.wrappedTileCoord[0], c, t.viewState.projection);
		p.canvas.width = y[0];
		p.canvas.height = y[1];
		const E = c / m;
		if (E !== 1) {
			const P = Xh(this.tmpTransform_);
			Er(P, E, E);
			p.setTransform.apply(p, P);
		}
		const x = f.getTileCoordExtent(e.wrappedTileCoord, this.tempExtent), T = m / _, v = Xh(this.tmpTransform_);
		Er(v, T, -T);
		mu(v, -x[0], -x[3]);
		for (let P = 0, S = e.executorGroups[O(i)].length; P < S; ++P) e.executorGroups[O(i)][P].execute(p, [p.canvas.width * E, p.canvas.height * E], v, 0, true, IC[i.getRenderMode()], null);
		s.renderedTileResolution = e.wantedResolution;
	}
}
class AC extends Pp {
	constructor(e) {
		e = e || {};
		const t = Object.assign({}, e);
		delete t.preload;
		const i = e.cacheSize === "undefined" ? 0 : e.cacheSize;
		delete e.cacheSize;
		delete t.useInterimTilesOnError;
		super(t);
		this.on;
		this.once;
		this.un;
		this.cacheSize_ = i;
		const s = e.renderMode || "hybrid";
		ee(s == "hybrid" || s == "vector", "`renderMode` must be `'hybrid'` or `'vector'`");
		this.renderMode_ = s;
		this.setPreload(e.preload ? e.preload : 0);
		this.setUseInterimTilesOnError(e.useInterimTilesOnError !== "undefined" ? e.useInterimTilesOnError : true);
		this.getBackground;
		this.setBackground;
	}
	createRenderer() {
		return new FC(this, { cacheSize: this.cacheSize_ });
	}
	getFeatures(e) {
		return super.getFeatures(e);
	}
	getFeaturesInExtent(e) {
		return this.getRenderer().getFeaturesInExtent(e);
	}
	getRenderMode() {
		return this.renderMode_;
	}
	getPreload() {
		return this.get(Ti.PRELOAD);
	}
	getUseInterimTilesOnError() {
		return this.get(Ti.USE_INTERIM_TILES_ON_ERROR);
	}
	setPreload(e) {
		this.set(Ti.PRELOAD, e);
	}
	setUseInterimTilesOnError(e) {
		this.set(Ti.USE_INTERIM_TILES_ON_ERROR, e);
	}
}
export { UC as $, ay as A, ey as B, Al as C, ZE as D, Ry as E, sl as F, Wy as G, Py as H, Nl as I, xy as J, py as K, Nr as L, yl as M, tT as N, Fl as O, wn as P, If as Q, O as R, yi as S, Ol as T, qE as U, l_ as V, kx as W, YE as X, eR as Y, pd as Z, bC as _, OC as a, Xi as a0, J as a1, Ce as a2, lh as a3, hd as a4, Zf as a5, TT as a6, cT as a7, gT as a8, wC as a9, CT as aa, Hc as ab, je as ac, wp as ad, h0 as ae, kC as af, Ud as ag, AC as ah, _T as ai, Z0 as aj, at as ak, lT as al, Pp as am, tt as an, M as ao, RT as ap, $d as aq, ri as ar, dR as as, Wx as at, Gr as b, pl as c, Pl as d, Il as e, Ll as f, $ as g, _s as h, me as i, pg as j, yg as k, ye as l, Fp as m, ky as n, FE as o, Dy as p, Ep as q, DC as r, Fy as s, BC as t, NC as u, Eu as v, H as w, On as x, hy as y, vy as z };
