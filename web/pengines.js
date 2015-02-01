/*  Copyright (c) 2014, Torbjörn Lager
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

function Pengine(options) {
    if ( Pengine.ids === undefined ) {
        Pengine.ids = [];
    }
    var src = options.src ? [options.src] : [];
    var format = options.format ? options.format : "json";
    var server = options.server !== undefined ? options.server : "/pengine";
    this.options = options;
    this.id = null;
    var that = this;
    // Private functions
    function source() {
        var scripts = document.getElementsByTagName('script');
        for (var i = 0; i < scripts.length; i++) {
            if (scripts[i].getAttribute('type') == 'text/x-prolog') {
                src.push(scripts[i].textContent);
            }
        }
        return src;
    }
    function options_to_list(options) {
        var opts = "[";
        for (var i in options) {
            opts += i + "(" + options[i] + "),";
        }
        if (opts.length > 1) {
            opts = opts.slice(0, -1);
        }
        return opts + "]";
    }
    function process_response(obj) {
        obj.pengine = that;
        if (obj.event === 'create') {
	    that.id = obj.id;
	    Pengine.ids.push(obj.id);
            if (Pengine.ids.length > obj.slave_limit) {
		that.destroy();
		obj.data = "Attempt to create too many pengines. "+
		           "The limit is: " + obj.slave_limit;
		obj.code = "too_many_pengines";
		if (options.onerror)
		    options.onerror.call(obj, obj);
		else if ( console !== undefined )
		    console.error(obj.data);
		else
		    alert(obj.data);
	    } else {
		if (options.oncreate) options.oncreate.call(obj, obj);
		if (obj.answer) process_response(obj.answer);
	    }
        } else if (obj.event === 'stop') {
            if (options.onstop) options.onstop.call(obj, obj);
        } else if (obj.event === 'success') {
            if (options.onsuccess) options.onsuccess.call(obj, obj);
        } else if (obj.event === 'failure') {
            if (options.onfailure) options.onfailure.call(obj, obj);
        } else if (obj.event === 'error') {
	    if ( obj.code == "existence_error" &&
		 obj.arg1 == "pengine" &&
		 obj.arg2 == that.id )
	      unregisterPengine(that.id);
            if (options.onerror)
	        options.onerror.call(obj, obj);
	    else if ( console !== undefined )
	        console.error(obj.data);
        } else if (obj.event === 'output') {
            if (options.onoutput) options.onoutput.call(obj, obj);
            that.pull_response(obj.id);
        } else if (obj.event === 'debug') {
            if (options.ondebug)
	        options.ondebug.call(obj, obj);
	    else if ( console !== undefined )
		console.log(obj.data);
            that.pull_response(obj.id);
        } else if (obj.event === 'prompt') {
            if (options.onprompt) options.onprompt.call(obj, obj);
        } else if (obj.event === 'abort') {
	    that.aborted = true;
            if (options.onabort) options.onabort.call(obj, obj);
        } else if (obj.event === 'destroy') {
	    unregisterPengine(that.id);
	    if (obj.data) process_response(obj.data);
            if (options.ondestroy) options.ondestroy.call(obj, obj);
        } else if (obj.event === 'died') {
	    unregisterPengine(that.id);
	    if ( !that.aborted ) {
	        obj.data = "Pengine has died";
		obj.code = "died";
	        if (options.onerror)
		    options.onerror.call(obj, obj);
		else if ( console !== undefined )
		    console.error(obj.data);
	    }
	}
    };
    /**
     * Process the reply to a `pull_response`.  If the last answer was
     * final, this question will be asked to a death pengine.  We do not
     * consider this an error.
     */
    function process_pull_response(obj) {
        obj.pengine = that;
	if ( obj.event !== 'died')
	    process_response(obj);
    }

    function send(event) {
        var event = encodeURIComponent(event);
        $.get(server + '/send?id=' + that.id +
	      '&event=' + event + '&format=' + format, process_response);
    }

    function unregisterPengine(id) {
      var index = Pengine.ids.indexOf(id);
      if ( index > -1 ) Pengine.ids.splice(index, 1);
    }


    // Public methods
    Pengine.prototype = {
        ask: function(query, options) {
	    send('ask((' + query + '), ' + options_to_list(options) + ')');
	},
	next: function(n) {
	    if ( n === undefined )
	      send('next');
	    else
	      send('next('+n+')');
	},
        stop: function() {
	    send('stop');
	},
	respond: function(input) {
	    send('input((' + input + '))');
	},
        pull_response: function(id) {
	    if ( id === undefined ) id = that.id;
	    $.get(server + '/pull_response?id=' + id +
		  '&format=' + format, process_pull_response);
	},
	abort: function() {
	    $.get(server + '/abort?id=' + that.id +
		  '&format=' + format, process_response);
	},
        destroy: function() {
	    send('destroy');
	}
    };
    // JW: Why is this needed!?
    $.extend(this, Pengine.prototype);

    Pengine.destroy_all = function(async) {
        if ( Pengine.ids.length > 0 ) {
	    $.ajax({ url:server + '/destroy_all?ids=' + Pengine.ids,
	             async: async === undefined ? true : false
		   });
	    Pengine.ids = [];
	}
    }
    // On creation
    var createOptions = {};
    createOptions["src_text"] = source().join('\n');
    createOptions["format"] = format;
    if (options.application) createOptions["application"] = options.application;
    if (options.ask) createOptions["ask"] = options.ask;
    if (options.template) createOptions["template"] = options.template;
    if (options.chunk) createOptions["chunk"] = options.chunk;
    if (typeof options.destroy == "boolean" )
      createOptions["destroy"] = options.destroy;
    $.ajax(server + '/create',
	   { "contentType": "application/json; charset=utf-8",
	     "dataType": "json",
	     "data": JSON.stringify(createOptions),
	     "success": process_response,
	     "type": "POST"
	   });
}/*end of Pengine()*/


/**
 * Serialize JavaScript data as a Prolog term.  The serialization
 * is performed according to the rules below:
 *
 *   - A number is serialized trivially
 *	 - A string is serialized into a Prolog string unless
 *	   `option.string == "atom"`
 *   - An array is serialized into a Prolog list
 *   - An object is serialized into a Prolog dict.  Keys that are
 *     integers are mapped to Prolog integers, other keys are mapped
 *     to Prolog atoms.  Note that in JavaScript {1:42} and {"1":42}
 *     are equavalent and both can be retrieved as either
 *     obj[1] or obj["1"]
 *
 * @param  {any} data is the data to be serialized
 * @param  {Object} [options]
 * @param {String} [options.string] If `"atom"`, translate
 * JavaScript strings into Prolog objects.
 * @return {String|undefined} is the Prolog serialization of `data`
 */

Pengine.stringify = function(data, options) {
  var msg  = "";
  var strq = options && options.string == "atom" ? "'" : '"';

  function serialize(data) {
    function stringEscape(s, q) {
      function dec2unicode(i)
      { var r;
	if      (i >= 0    && i <= 15)    { r = "\\u000" + i.toString(16); }
	else if (i >= 16   && i <= 255)   { r = "\\u00"  + i.toString(16); }
	else if (i >= 256  && i <= 4095)  { r = "\\u0"   + i.toString(16); }
	else if (i >= 4096 && i <= 65535) { r = "\\u"    + i.toString(16); }
	return r
      }

      var result = q;
      for(var i=0; i<s.length; i++) {
	var c = s.charAt(i);
	if ( c >= ' ' ) {
	       if ( c == '\\' ) result += "\\\\";
	  else if ( c ==   q  ) result += "\\"+q;
	  else result += c;
	} else {
	       if ( c == '\n' ) result += "\\n";
	  else if ( c == '\r' ) result += "\\r";
	  else if ( c == '\t' ) result += "\\t";
	  else if ( c == '\b' ) result += "\\b";
	  else if ( c == '\f' ) result += "\\f";
	  else result += dec2unicode(c.charCodeAt(0));
	}
      }
      return result+q;
    }

    function serializeKey(k) {
      if ( k.match(/^\d+$/) ) {
	msg += k;
      } else {
	msg += stringEscape(k, "'");
      }

      return true;
    }

    switch ( typeof(data) ) {
      case "number":
	msg += data;
	break;
      case "string":
	msg += stringEscape(data, strq);
	break;
      case "object":
	if ( Array.isArray(data) ) {
	  msg += "[";
	  for(var i=0; i<data.length; i++) {
	    if ( !serialize(data[i]) )
	      return false;
	    if ( i+1 < data.length )
	      msg += ",";
	  }
	  msg += "]";
	} else {
	  var first = true;
	  msg += "js{";
	  for(var p in data) {
	    if ( data.hasOwnProperty(p) ) {
	      if ( !first )
		msg += ",";
	      else
		first = false;
	      if ( !serializeKey(p) )
		return false;
	      msg += ":";
	      if ( !serialize(data[p]) )
		return false;
	    }
	  }
	  msg += "}";
	}
	break;
      default:
	return false;
    }

    return true;
  }

  if ( serialize(data) )
    return msg;
}/*end of Pengine.stringify*/

window.onunload = function() {
    try {
        Pengine.destroy_all();
    } catch(e) {}
};
