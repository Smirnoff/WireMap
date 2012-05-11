function request_url(type, url, callback, mime_type, err, data)
{
  if (err === undefined) err = function(val) { alert("Error: " + val); };

  if (data === undefined) data = null;

  var request = new XMLHttpRequest();
  request.open(type, url, true);
  request.onreadystatechange =
    function(req_event)
      {
        if (request.status === 200) callback(request.responseText);
        else err(request.statusText);
      };
  request.overrideMimeType(mime_type);
  request.send(data);
}

function post_to_url(url, callback, data, err, mime_type)
{
  if (mime_type === undefined) mime_type = "text/plain";
  request_url('POST', url, callback, mime_type, err, data);
}

function check_iban(iban, res, err)
{
  var been_called = false;
  post_to_url('iban_checker.scm',
              function(x)
                {
                  if (x.length != 0 && !been_called)
                    {
                      been_called = true;
                      res(JSON.parse(x));
                    }
                },
              JSON.stringify({iban: iban}),
              err);
}

// page stuff

var res_ids = ["res_note",
               "res_country_code", "res_country_name",
               "res_bank_id", "res_branch_id"];

function fetch_element_by_id(id)
{
  return document.getElementById(id);
}

function element_set_by_id(id, value)
{
  fetch_element_by_id(id).innerHTML = value;
}

function clear_results()
{
  res_ids.forEach(function(id) { element_set_by_id(id, ""); });
}

function is_gapped(iban, gap)
{
  if (gap === undefined) gap = 4;
  var idx;
  for (idx = 0; idx < iban.length; idx++)
    {
      if ((idx % 5 == 4) && iban[idx] != " ") return false;
      if ((idx % 5 != 4) && iban[idx] == " ") return false;
    }

  return true;
}

function ensure_gapped(iban, gap)
{
  if (gap === undefined) gap = 4;

  if (is_gapped(iban, gap)) return false;

  var temp = iban;
  while (temp.indexOf(" ") != -1) temp = temp.replace(" ", "");

  var res = temp.slice(0, Math.min(gap, temp.length));
  temp = temp.slice(res.length);

  while (temp.length > 0)
    {
      var len = Math.min(gap, temp.length);
      res += " " + temp.slice(0, len);
      temp = temp.slice(len);
    }

  return res;
}

function click_check_iban()
{
  var iban = document.getElementById("iban").value;
  check_iban(
    iban,
    function(x)
      {
        clear_results();
        console.log(x); // debug

        if (x["message"]) element_set_by_id("res_note", x["message"]);
        else
          {
            element_set_by_id(
              "res_country_code",
              "ISO 3166-1 alpha-2 Country Code: " + x["country-code"]);
            element_set_by_id(
              "res_country_name",
              "Country Name: " + x["country-name"]);
            element_set_by_id(
              "res_bank_id",
              "Bank ID: " + x["bank-identifier"]);
            if (x["branch-identifier"])
              element_set_by_id(
                "res_branch_id",
                "Branch ID: " + x["branch-identifier"]);
          }

        return;
      });
}
