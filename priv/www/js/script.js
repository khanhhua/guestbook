$.fn.api.settings.api = {
  'leave message': '/rest/messages'
};

$('#modalContact .ui.send.button').api({
  action: 'leave message',
  method : 'POST',
  beforeSend: function (settings) {
    settings.contentType = 'application/json';
    settings.data = JSON.stringify({
      name:    $('#modalContact [name=username]').val(),
      contact: $('#modalContact [name=contact]').val(),
      message: $('#modalContact [name=message]').val()
    });

    return settings;
  },
  onSuccess: function () {
    $('#modalContact').modal('hide');
  }
});
