var _user$project$Native_JsonSchema = function () {
  var Ajv = require(process.env.ELM_JSON_SCHEMA_AJV_PATH);

  function validate(schemaString, jsonString){
    var ajv = new Ajv();
    try {
      var valid = ajv.validate(JSON.parse(schemaString), JSON.parse(jsonString));
    } catch(e) {
      return { ctor: "Err" , _0: e.toString() };
    }
    if (!valid) {
      var message = ajv
	.errors
	.map(function(e) { return e.message; })
	.join("\n");
      return { ctor: "Err" , _0: message };
    };
    return { ctor: "Ok" , _0: [] };
  }

  return {
    validate: F2(validate)
  };
}();
