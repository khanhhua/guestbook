(function ($) {
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
  $('.button.leave-message').on('click', function () {
    $('#modalContact').modal('show');
  });

  $(renderMessageList);

  function renderMessageList () {
    $.get('/partials/message.item.html').then(function (template) {
      console.log(template);

      var templateFn = _.template(template);
      syncMessages().then(function (data) {
        var $messageList = $('.message-list');

        var $html = _.map(data.messages, function (item) {
          item.username = (_.find(data.guests, function (x) {
            return x.id === item.guest_id;
          })).username;
          return templateFn(item);
        });
        $messageList.prepend($html);
      });
    })
  }

  function syncMessages () {
    return $.get('/rest/messages').then(function (data) {
      return data;
    });
  }
})(jQuery);
