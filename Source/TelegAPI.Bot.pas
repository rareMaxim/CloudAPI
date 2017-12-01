unit TelegAPI.Bot;

interface

uses
  System.Rtti,
  TelegAPI.Types,
  TelegAPI.Types.Enums,
  TelegAPI.Types.InlineQueryResults,
  TelegAPI.Types.ReplyMarkups,
  TelegAPI.Types.Impl,
  TelegaPi.Exceptions;

type
  ITelegramBot = interface
    ['{12FA5CF8-3723-4ED1-BC1F-F1643B4FA361}']
      // private
    function GetToken: string;
    procedure SetToken(const Value: string);
    function GetExceptionManager: ItgExceptionHandler;
    procedure SetExceptionManager(const Value: ItgExceptionHandler);
      // public
{$REGION 'Getting updates'}
     /// <summary>
     /// <para>
     /// Используйте этот метод для получения обновлений используя long
     /// polling.
     /// </para>
     /// <para>
     /// Use this method to receive incoming updates using long polling.
     /// </para>
     /// </summary>
     /// <param name="Offset">
     /// Identifier of the first update to be returned. Must be greater by one
     /// than the highest among the identifiers of previously received
     /// updates. By default, updates starting with the earliest unconfirmed
     /// update are returned. An update is considered confirmed as soon as <see cref="TelegAPI.Bot|TTelegramBot.GetUpdates(Int64,Int64,Int64,TAllowedUpdates)">
     /// getUpdates</see> is called with an offset higher than its update_id.
     /// The negative offset can be specified to retrieve updates starting
     /// from -offset update from the end of the updates queue. All previous
     /// updates will forgotten.
     /// </param>
     /// <param name="Limit">
     /// Количество обновлений которые могут прийти в одном запросе.
     /// Допустимое значение от 1 до 100. По умолчанию - 100.Limits the number
     /// of updates to be retrieved. Values between 1—100 are accepted.
     /// Defaults to 100.
     /// </param>
     /// <param name="Timeout">
     /// Timeout in seconds for long polling. Defaults to 0, i.e. usual short
     /// polling
     /// </param>
     /// <param name="AllowedUpdates">
     /// List the types of updates you want your bot to receive. For example,
     /// specify [“message”, “edited_channel_post”, “callback_query”] to only
     /// receive updates of these types. See Update for a complete list of
     /// available update types. Specify an empty list to receive all updates
     /// regardless of type (default). If not specified, the previous setting
     /// will be used. <br /><br />Please note that this parameter doesn't
     /// affect updates created before the call to the getUpdates, so unwanted
     /// updates may be received for a short period of time.
     /// </param>
     /// <returns>
     /// An Array of Update objects is returned.
     /// </returns>
     /// <remarks>
     /// 1. This method will not work if an outgoing webhook is set up. 2. In
     /// order to avoid getting duplicate updates, recalculate offset after
     /// each server response.
     /// </remarks>
    function GetUpdates(const Offset: Int64 = 0; const Limit: Int64 = 100; const Timeout: Int64 = 0; AllowedUpdates: TAllowedUpdates = UPDATES_ALLOWED_ALL): TArray<ItgUpdate>;
     /// <summary>
     /// Use this method to specify a url and receive incoming updates via an
     /// outgoing webhook. Whenever there is an update for the bot, we will
     /// send an HTTPS POST request to the specified url, containing a
     /// JSON-serialized Update. In case of an unsuccessful request, we will
     /// give up after a reasonable amount of attempts.
     /// </summary>
     /// <param name="Url">
     /// HTTPS url to send updates to. Use an empty string to remove webhook
     /// integration
     /// </param>
     /// <param name="Certificate">
     /// Upload your public key certificate so that the root certificate in
     /// use can be checked. See our self-signed guide for details.
     /// </param>
     /// <param name="MaxConnections">
     /// Maximum allowed number of simultaneous HTTPS connections to the
     /// webhook for update delivery, 1-100. Defaults to 40. Use lower values
     /// to limit the load on your bot‘s server, and higher values to increase
     /// your bot’s throughput.
     /// </param>
     /// <param name="AllowedUpdates">
     /// List the types of updates you want your bot to receive. For example,
     /// specify [“message”, “edited_channel_post”, “callback_query”] to only
     /// receive updates of these types. See Update for a complete list of
     /// available update types. Specify an empty list to receive all updates
     /// regardless of type (default). If not specified, the previous setting
     /// will be used. <br /><br />Please note that this parameter doesn't
     /// affect updates created before the call to the setWebhook, so unwanted
     /// updates may be received for a short period of time.
     /// </param>
     /// <remarks>
     /// <para>
     /// Notes
     /// </para>
     /// <para>
     /// 1. You will not be able to receive updates using <see cref="TelegAPI.Bot|TTelegramBot.GetUpdates(Int64,Int64,Int64,TAllowedUpdates)">
     /// getUpdates</see> for as long as an outgoing webhook is set up.
     /// </para>
     /// <para>
     /// 2. To use a self-signed certificate, you need to upload your <see href="https://core.telegram.org/bots/self-signed">
     /// public key certificate</see> using <c>certificate</c> parameter.
     /// Please upload as InputFile, sending a String will not work.
     /// </para>
     /// <para>
     /// 3. Ports currently supported for Webhooks: <b>443, 80, 88, 8443</b>
     /// .
     /// </para>
     /// <para>
     /// <b>NEW!</b> If you're having any trouble setting up webhooks,
     /// please check out this <see href="https://core.telegram.org/bots/webhooks">
     /// amazing guide to Webhooks</see>.
     /// </para>
     /// </remarks>
    function SetWebhook(const Url: string; Certificate: TtgFileToSend = nil; MaxConnections: Int64 = 40; AllowedUpdates: TAllowedUpdates = UPDATES_ALLOWED_ALL): Boolean;
     /// <summary>
     /// Use this method to remove webhook integration if you decide to switch
     /// back to <see cref="TelegAPI.Bot|TTelegramBot.GetUpdates(Int64,Int64,Int64,TAllowedUpdates)">
     /// getUpdates</see>.
     /// </summary>
     /// <returns>
     /// Returns <c>True</c> on success.
     /// </returns>
    function DeleteWebhook: Boolean;

     /// <summary>
     /// Use this method to get current webhook status.
     /// </summary>
     /// <returns>
     /// On success, returns a <see cref="TelegAPi.Types|TtgWebhookInfo">
     /// WebhookInfo</see> object
     /// </returns>
     /// <remarks>
     /// If the bot is using <see cref="TelegAPI.Bot|TTelegramBot.GetUpdates(Int64,Int64,Int64,TAllowedUpdates)">
     /// getUpdates</see>, will return an object with the url field empty
     /// </remarks>
    function GetWebhookInfo: ItgWebhookInfo;
{$ENDREGION}
{$REGION 'Basic methods'}
    /// <summary>
    /// <para>
    /// Простой метод для проверки токена вашего бота
    /// </para>
    /// <para>
    /// A simple method for testing your bot's auth token.
    /// </para>
    /// </summary>
    /// <returns>
    /// <para>
    /// Возвращает основную информацию о боте
    /// </para>
    /// <para>
    /// Returns basic information about the bot in form of a <see cref="TelegAPi.Types|TtgUser">
    /// User</see> object.
    /// </para>
    /// </returns>
    function GetMe: ItgUser;
     /// <summary>
     /// Use this method to send text messages.
     /// </summary>
     /// <param name="ChatId">
     /// Int64 or String. Unique identifier for the target chat or username of
     /// the target channel (in the format <c>@channelusername</c> ).
     /// </param>
     /// <param name="Text">
     /// Text of the message to be sent
     /// </param>
     /// <param name="ParseMode">
     /// Send Markdown or HTML, if you want Telegram apps to show bold,
     /// italic, fixed-width text or inline URLs in your bot's message.
     /// </param>
     /// <param name="DisableWebPagePreview">
     /// Disables link previews for links in this message
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound.
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message
     /// </param>
     /// <param name="ReplyMarkup">
     /// InlineKeyboardMarkup or ReplyKeyboardMarkup or ReplyKeyboardHide or
     /// ForceReply. Additional interface options. A JSON-serialized object
     /// for an inline keyboard, custom reply keyboard, instructions to hide
     /// reply keyboard or to force a reply from the user.
     /// </param>
     /// <returns>
     /// On success, the sent <see cref="TelegAPi.Types|TtgMessage">Message</see>
     /// is returned.
     /// </returns>
    function SendMessage(const ChatId: TValue; const Text: string; ParseMode: TtgParseMode = TtgParseMode.Default; DisableWebPagePreview: Boolean = False; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
     /// <summary>
     /// Use this method to forward messages of any kind.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="FromChatId">
     /// Unique identifier for the chat where the original message was sent
     /// (or channel username in the format @channelusername) <br />
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound. <br />
     /// </param>
     /// <param name="MessageId">
     /// Unique message identifier <br />
     /// </param>
     /// <returns>
     /// On success, the sent Message is returned.
     /// </returns>
    function ForwardMessage(ChatId: TValue; FromChatId: TValue; MessageId: Int64; DisableNotification: Boolean = False): ITgMessage;
     /// <summary>
     /// Use this method to send photos.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="Photo">
     /// Photo to send. You can either pass a file_id as String to resend a
     /// photo that is already on the Telegram servers, or upload a new photo
     /// using multipart/form-data. <br />
     /// </param>
     /// <param name="Caption">
     /// Photo caption (may also be used when resending photos by file_id),
     /// 0-200 characters <br />
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound. <br />
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// Additional interface options. A JSON-serialized object for an inline
     /// keyboard, custom reply keyboard, instructions to remove reply
     /// keyboard or to force a reply from the user. <br />
     /// </param>
     /// <returns>
     /// On success, the sent <see cref="TelegAPi.Types|TtgMessage">Message</see>
     /// is returned.
     /// </returns>
     /// <example>
     /// <code lang="Delphi">var
     /// LMessage: TtgMessage;
     /// Begin
     /// //Если не известен ИД файла
     /// LMessage := sendPhoto(chatId, TtgFileToSend.Create('Путь к файлу'), nil);
     /// //Если известен ИД файла
     /// LMessage := sendPhoto(chatId, 'ИД Файла');
     /// ...
     /// LMessage.Free;
     /// End; </code>
     /// </example>
    function SendPhoto(ChatId: TValue; Photo: TValue; const Caption: string = ''; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
     /// <summary>
     /// Use this method to send audio files, if you want Telegram clients to
     /// display them in the music player. Your audio must be in the .mp3
     /// format.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="Audio">
     /// Audio file to send. You can either pass a file_id as String to resend
     /// an audio that is already on the Telegram servers, or upload a new
     /// audio file using multipart/form-data. <br />
     /// </param>
     /// <param name="Duration">
     /// Duration of the audio in seconds <br />
     /// </param>
     /// <param name="Performer">
     /// Performer <br />
     /// </param>
     /// <param name="Title">
     /// Track name <br />
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound. <br />
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// Additional interface options. A JSON-serialized object for an inline
     /// keyboard, custom reply keyboard, instructions to hide reply keyboard
     /// or to force a reply from the user. <br />
     /// </param>
     /// <returns>
     /// On success, the sent <see cref="TelegAPi.Types|ITgMessage">Message</see>
     /// is returned.
     /// </returns>
     /// <remarks>
     /// Bots can currently send audio files of up to 50 MB in size, this
     /// limit may be changed in the future. For sending voice messages, use
     /// the <see cref="TelegAPI.Bot|TTelegramBot.SendVoice(TValue,TValue,Int64,Boolean,Int64,IReplyMarkup)">
     /// sendVoice</see> method instead.
     /// </remarks>
    function SendAudio(ChatId: TValue; Audio: TValue; const Caption: string = ''; Duration: Int64 = 0; const Performer: string = ''; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
     /// <summary>
     /// Use this method to send general files.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="Document">
     /// File to send. You can either pass a file_id as String to resend a
     /// file that is already on the Telegram servers, or upload a new file
     /// using multipart/form-data. <br />
     /// </param>
     /// <param name="Caption">
     /// Document caption (may also be used when resending documents by
     /// file_id), 0-200 characters <br />
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound. <br />
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// Additional interface options. A JSON-serialized object for an inline
     /// keyboard, custom reply keyboard, instructions to hide reply keyboard
     /// or to force a reply from the user. <br />
     /// </param>
     /// <returns>
     /// On success, the sent <see cref="TelegAPi.Types|ITgMessage">Message</see>
     /// is returned.
     /// </returns>
     /// <remarks>
     /// Bots can currently send files of any type of up to 50 MB in size,
     /// this limit may be changed in the future.
     /// </remarks>
    function SendDocument(ChatId: TValue; Document: TValue; const Caption: string = ''; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
     /// <summary>
     /// Use this method to send video files, Telegram clients support mp4
     /// videos (other formats may be sent as Document).
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="Video">
     /// Video to send. You can either pass a file_id as String to resend a
     /// video that is already on the Telegram servers, or upload a new video
     /// file using multipart/form-data. <br />
     /// </param>
     /// <param name="Duration">
     /// Duration of sent video in seconds <br />
     /// </param>
     /// <param name="Width">
     /// Video width <br />
     /// </param>
     /// <param name="Height">
     /// Video height <br />
     /// </param>
     /// <param name="Caption">
     /// Video caption (may also be used when resending videos by file_id),
     /// 0-200 characters <br />
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound. <br />
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// Additional interface options. A JSON-serialized object for an inline
     /// keyboard, custom reply keyboard, instructions to hide reply keyboard
     /// or to force a reply from the user. <br />
     /// </param>
     /// <returns>
     /// On success, the sent Message is returned.
     /// </returns>
     /// <remarks>
     /// Bots can currently send video files of up to 50 MB in size, this
     /// limit may be changed in the future.
     /// </remarks>
    function SendVideo(ChatId: TValue; Video: TValue; const Caption: string = ''; Duration: Int64 = 0; Width: Int64 = 0; Height: Int64 = 0; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;

     /// <summary>
     /// Use this method to send audio files, if you want Telegram clients to
     /// display the file as a playable voice message. For this to work, your
     /// audio must be in an .ogg file encoded with OPUS (other formats may be
     /// sent as Audio or Document).
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="Voice">
     /// Audio file to send. You can either pass a file_id as String to resend
     /// an audio that is already on the Telegram servers, or upload a new
     /// audio file using multipart/form-data. <br />
     /// </param>
     /// <param name="Duration">
     /// Duration of sent audio in seconds <br />
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound. <br />
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// Additional interface options. A JSON-serialized object for an inline
     /// keyboard, custom reply keyboard, instructions to hide reply keyboard
     /// or to force a reply from the user. <br />
     /// </param>
     /// <returns>
     /// On success, the sent <see cref="TelegAPi.Types|ITgMessage">Message</see>
     /// is returned
     /// </returns>
     /// <remarks>
     /// Bots can currently send voice messages of up to 50 MB in size, this
     /// limit may be changed in the future.
     /// </remarks>
    function SendVoice(ChatId: TValue; Voice: TValue; const Caption: string = ''; Duration: Int64 = 0; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
     /// <summary>
     /// As of <see href="https://telegram.org/blog/video-messages-and-telescope">
     /// v.4.0</see>, Telegram clients support rounded square mp4 videos of up
     /// to 1 minute long.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="VideoNote">
     /// Video note to send. Pass a file_id as String to send a video note
     /// that exists on the Telegram servers (recommended) or upload a new
     /// video using multipart/form-data. More info on Sending Files ».
     /// Sending video notes by a URL is currently unsupported <br />
     /// </param>
     /// <param name="Duration">
     /// Duration of sent video in seconds <br />
     /// </param>
     /// <param name="Length">
     /// Video width and height <br />
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound. <br />
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// Additional interface options. A JSON-serialized object for an inline
     /// keyboard, custom reply keyboard, instructions to remove reply
     /// keyboard or to force a reply from the user. <br />
     /// </param>
     /// <returns>
     /// On success, the sent <see cref="TelegAPi.Types|ITgMessage">Message</see>
     /// is returned.
     /// </returns>
     /// <remarks>
     /// Use this method to send video messages.
     /// </remarks>
    function SendVideoNote(ChatId: TValue; //
      VideoNote: TValue; //
      Duration: Int64 = 0; //
      Length: Int64 = 0; //
      DisableNotification: Boolean = False; //
      ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil //
    ): ITgMessage;

     /// <summary>
     /// Use this method to send point on the map.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="Location">
     /// Latitude and Longitude of location
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message <see href="https://telegram.org/blog/channels-2-0#silent-messages">
     /// silently</see>. iOS users will not receive a notification, Android
     /// users will receive a notification with no sound. <br />
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// Additional interface options. A JSON-serialized object for an inline
     /// keyboard, custom reply keyboard, instructions to hide reply keyboard
     /// or to force a reply from the user. <br />
     /// </param>
     /// <returns>
     /// On success, the sent <see cref="TelegAPi.Types|ITgMessage">Message</see>
     /// is returned.
     /// </returns>
    function SendLocation(ChatId: TValue; Location: TtgLocation; LivePeriod: Int64 = 0; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
     /// <summary>
     /// Use this method to send information about a venue.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="Venue">
     /// Latitude and Longitude of the venue <br />
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound. <br /><br />
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// Additional interface options. A JSON-serialized object for an inline
     /// keyboard, custom reply keyboard, instructions to hide reply keyboard
     /// or to force a reply from the user. <br />
     /// </param>
     /// <param name="title">
     /// Name of the venue
     /// </param>
     /// <param name="address">
     /// Address of the venue
     /// </param>
     /// <param name="foursquare_id">
     /// Foursquare identifier of the venue
     /// </param>
     /// <returns>
     /// On success, the sent Message is returned.
     /// </returns>
    function SendVenue(ChatId: TValue; Venue: TtgVenue; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
     /// <summary>
     /// Use this method to send phone contacts.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="Contact">
     /// Contact's phone number, first name, last name
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound.
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message
     /// </param>
     /// <param name="ReplyMarkup">
     /// Additional interface options. A JSON-serialized object for an inline
     /// keyboard, custom reply keyboard, instructions to hide keyboard or to
     /// force a reply from the user. <br />
     /// </param>
     /// <returns>
     /// On success, the sent <see cref="TelegAPi.Types|ITgMessage">Message</see>
     /// is returned.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#sendcontact" />
    function SendContact(ChatId: TValue; Contact: TtgContact; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
     /// <summary>
     /// Use this method when you need to tell the user that something is
     /// happening on the bot's side. The status is set for 5 seconds or less
     /// (when a message arrives from your bot, Telegram clients clear its
     /// typing status).
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="Action">
     /// Type of action to broadcast. Choose one, depending on what the user
     /// is about to receive: typing for text messages, upload_photo for
     /// photos, record_video or upload_video for videos, record_audio or
     /// upload_audio for audio files, upload_document for general files,
     /// find_location for location data <br />
     /// </param>
     /// <remarks>
     /// We only recommend using this method when a response from the bot will
     /// take a noticeable amount of time to arrive.
     /// </remarks>
     /// <seealso href="https://core.telegram.org/bots/api#sendchataction" />
    function SendChatAction(ChatId: TValue; const Action: TtgSendChatAction): Boolean;
     /// <summary>
     /// Use this method to get a list of profile pictures for a user.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier of the target user <br />
     /// </param>
     /// <param name="Offset">
     /// Sequential number of the first photo to be returned. By default, all
     /// photos are returned. <br />
     /// </param>
     /// <param name="Limit">
     /// Limits the number of photos to be retrieved. Values between 1—100 are
     /// accepted. Defaults to 100. <br />
     /// </param>
     /// <returns>
     /// Returns a <see cref="TelegAPi.Types|TtgUserProfilePhotos">
     /// UserProfilePhotos</see> object.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#getuserprofilephotos" />
    function GetUserProfilePhotos(ChatId: TValue; Offset: Int64; Limit: Int64 = 100): ItgUserProfilePhotos;
     /// <summary>
     /// Use this method to get basic info about a file and prepare it for
     /// downloading. For the moment, bots can download files of up to 20MB in
     /// size.
     /// </summary>
     /// <param name="FileId">
     /// File identifier to get info about <br />
     /// </param>
     /// <returns>
     /// On success, a <see cref="TelegAPi.Types|TtgFile">File</see> object is
     /// returned.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#getfile" />
    function GetFile(const FileId: string): ItgFile;
     /// <summary>
     /// Use this method to kick a user from a group, a supergroup or a
     /// channel. In the case of supergroups and channels, the user will not
     /// be able to return to the group on their own using invite links, etc.,
     /// unless unbanned first. The bot must be an administrator in the chat
     /// for this to work and must have the appropriate admin rights.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target group or username of the target
     /// supergroup (in the format @supergroupusername) <br />
     /// </param>
     /// <param name="UserId">
     /// Unique identifier of the target user <br />
     /// </param>
     /// <param name="UntilDate">
     /// Date when the user will be unbanned, unix time. If user is banned for
     /// more than 366 days or less than 30 seconds from the current time they
     /// are considered to be banned forever <br />unbanChatMember
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
     /// <remarks>
     /// Note: In regular groups (non-supergroups), this method will only work
     /// if the ‘All Members Are Admins’ setting is off in the target group.
     /// Otherwise members may only be removed by the group's creator or by
     /// the member that added them.
     /// </remarks>
     /// <seealso href="https://core.telegram.org/bots/api#kickchatmember" />
    function KickChatMember(ChatId: TValue; UserId: Int64; UntilDate: Int64 = 0): Boolean;
     /// <summary>
     /// Use this method to unban a previously kicked user in a supergroup.
     /// The user will not return to the group automatically, but will be able
     /// to join via link, etc.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target group or username of the target
     /// supergroup (in the format @supergroupusername) <br />
     /// </param>
     /// <param name="UserId">
     /// Unique identifier of the target user <br />
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
     /// <remarks>
     /// The bot must be an administrator in the group for this to work.
     /// </remarks>
     /// <seealso href="https://core.telegram.org/bots/api#unbanchatmember" />
    function UnbanChatMember(ChatId: TValue; UserId: Int64): Boolean;
     /// <summary>
     /// Use this method for your bot to leave a group, supergroup or channel.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target group or username of the target
     /// supergroup (in the format @supergroupusername) <br />
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#leavechat" />
    function LeaveChat(ChatId: TValue): Boolean;
     /// <summary>
     /// Use this method to get up to date information about the chat (current
     /// name of the user for one-on-one conversations, current username of a
     /// user, group or channel, etc.)
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// supergroup or channel (in the format @channelusername) <br />
     /// </param>
     /// <returns>
     /// Returns a <see cref="TelegAPi.Types|TtgChat">Chat</see> object on
     /// success.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#getchat" />
    function GetChat(const ChatId: TValue): ItgChat;
     /// <summary>
     /// Use this method to get a list of administrators in a chat
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// supergroup or channel (in the format @channelusername) <br />
     /// </param>
     /// <returns>
     /// On success, returns an Array of <see cref="TelegAPi.Types|TtgChatMember">
     /// ChatMember</see> objects that contains information about all chat
     /// administrators except other bots. If the chat is a group or a
     /// supergroup and no administrators were appointed, only the creator
     /// will be returned.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#getchatadministrators" />
    function GetChatAdministrators(const ChatId: TValue): TArray<ItgChatMember>;
     /// <summary>
     /// Use this method to get the number of members in a chat.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// supergroup or channel (in the format @channelusername) <br />
     /// </param>
     /// <returns>
     /// Returns Int64 on success.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#getchatmemberscount" />
    function GetChatMembersCount(const ChatId: TValue): Int64;
     /// <summary>
     /// Use this method to get information about a member of a chat.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target group or username of the target
     /// supergroup (in the format @supergroupusername) <br />
     /// </param>
     /// <param name="UserId">
     /// Unique identifier of the target user <br />
     /// </param>
     /// <returns>
     /// Returns a <see cref="TelegAPi.Types|TtgChatMember">ChatMember</see>
     /// object on success.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#getchatmember" />
    function GetChatMember(ChatId: TValue; UserId: Int64): ItgChatMember;
     /// <summary>
     /// Use this method to send answers to callback queries sent from inline
     /// keyboards. The answer will be displayed to the user as a notification
     /// at the top of the chat screen or as an alert.
     /// </summary>
     /// <param name="CallbackQueryId">
     /// Unique identifier for the query to be answered <br />
     /// </param>
     /// <param name="Text">
     /// Text of the notification. If not specified, nothing will be shown to
     /// the user <br />
     /// </param>
     /// <param name="ShowAlert">
     /// If true, an alert will be shown by the client instead of a
     /// notification at the top of the chat screen. Defaults to false. <br />
     /// </param>
     /// <returns>
     /// On success, True is returned.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#answercallbackquery" />
    function AnswerCallbackQuery(const CallbackQueryId: string; const Text: string = ''; ShowAlert: Boolean = False; const Url: string = ''; CacheTime: Int64 = 0): Boolean;
{$ENDREGION}
{$REGION 'Updating messages'}
     /// <summary>
     /// Use this method to edit text messages sent by the bot or via the bot
     /// (for inline bots).
     /// </summary>
     /// <param name="ChatId">
     /// Required if inline_message_id is not specified. Unique identifier for
     /// the target chat or username of the target channel (in the format
     /// @channelusername) <br />
     /// </param>
     /// <param name="MessageId">
     /// Required if inline_message_id is not specified. Unique identifier of
     /// the sent message <br />
     /// </param>
     /// <param name="InlineMessageId">
     /// Required if chat_id and message_id are not specified. Identifier of
     /// the inline message <br />
     /// </param>
     /// <param name="Text">
     /// New text of the message <br />
     /// </param>
     /// <param name="ParseMode">
     /// Send Markdown or HTML, if you want Telegram apps to show bold,
     /// italic, fixed-width text or inline URLs in your bot's message. <br />
     /// </param>
     /// <param name="DisableWebPagePreview">
     /// Disables link previews for links in this message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// A JSON-serialized object for an inline keyboard. <br />
     /// </param>
     /// <returns>
     /// On success, if edited message is sent by the bot, the edited Message
     /// is returned, otherwise True is returned.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#editmessagetext" />
    function EditMessageText(ChatId: TValue; MessageId: Int64; const Text: string; ParseMode: TtgParseMode = TtgParseMode.Default; DisableWebPagePreview: Boolean = False; ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function EditMessageText(const InlineMessageId: string; const Text: string; ParseMode: TtgParseMode = TtgParseMode.Default; DisableWebPagePreview: Boolean = False; ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
     /// <summary>
     /// Use this method to edit captions of messages sent by the bot or via
     /// the bot (for inline bots).
     /// </summary>
     /// <param name="ChatId">
     /// Required if InlineMessageId is not specified. Unique identifier for
     /// the target chat or username of the target channel (in the format
     /// @channelusername) <br />
     /// </param>
     /// <param name="MessageId">
     /// Required if InlineMessageId is not specified. Unique identifier of <br />
     /// the sent message <br />
     /// </param>
     /// <param name="InlineMessageId">
     /// Required if ChatId and MessageId are not specified. Identifier of the
     /// inline message <br />
     /// </param>
     /// <param name="Caption">
     /// New caption of the message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// A JSON-serialized object for an inline keyboard. <br />
     /// </param>
     /// <returns>
     /// On success, if edited message is sent by the bot, the edited Message
     /// is returned, otherwise True is returned.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#editmessagereplymarkup" />
    function EditMessageCaption(ChatId: TValue; MessageId: Int64; const Caption: string; ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
     { TODO -oM.E.Sysoev -cGeneral : Create Documentatiom }
    function EditMessageCaption(const InlineMessageId: string; const Caption: string; ReplyMarkup: IReplyMarkup = nil): Boolean; overload;

     /// <summary>
     /// Use this method to edit live location messages sent by the bot or via
     /// the bot (for inline bots). A location can be edited until its
     /// live_period expires or editing is explicitly disabled by a call to
     /// stopMessageLiveLocation.
     /// </summary>
     /// <param name="ChatId">
     /// Required if inline_message_id is not specified. Unique identifier for
     /// the target chat or username of the target channel (in the format
     /// @channelusername)
     /// </param>
     /// <param name="MessageId">
     /// Required if inline_message_id is not specified. Identifier of the
     /// sent message
     /// </param>
     /// <param name="Location">
     /// new location
     /// </param>
     /// <param name="ReplyMarkup">
     /// A JSON-serialized object for a new inline keyboard.
     /// </param>
     /// <returns>
     /// On success, if the edited message was sent by the bot, the edited
     /// Message is returned, otherwise True is returned.
     /// </returns>
    function editMessageLiveLocation(ChatId: TValue; MessageId: Int64; Location: TtgLocation; ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
     /// <summary>
     /// Use this method to edit live location messages sent by the bot or via
     /// the bot (for inline bots). A location can be edited until its
     /// live_period expires or editing is explicitly disabled by a call to
     /// stopMessageLiveLocation.
     /// </summary>
     /// <param name="InlineMessageId">
     /// Required if chat_id and message_id are not specified. Identifier of
     /// the inline message
     /// </param>
     /// <param name="Location">
     /// new location
     /// </param>
     /// <param name="ReplyMarkup">
     /// A JSON-serialized object for a new inline keyboard.
     /// </param>
     /// <param name="ChatId">
     /// Required if inline_message_id is not specified. Unique identifier for
     /// the target chat or username of the target channel (in the format
     /// @channelusername)
     /// </param>
     /// <param name="MessageId">
     /// Required if inline_message_id is not specified. Identifier of the
     /// sent message
     /// </param>
     /// <returns>
     /// On success, if the edited message was sent by the bot, the edited
     /// Message is returned, otherwise True is returned.
     /// </returns>
    function editMessageLiveLocation(const InlineMessageId: string; Location: TtgLocation; ReplyMarkup: IReplyMarkup = nil): Boolean; overload;

     /// <summary>
     /// Use this method to stop updating a live location message sent by the
     /// bot or via the bot (for inline bots) before live_period expires.
     /// </summary>
     /// <param name="ChatId">
     /// equired if inline_message_id is not specified. Unique identifier for
     /// the target chat or username of the target channel (in the format
     /// @channelusername)
     /// </param>
     /// <param name="MessageId">
     /// Required if inline_message_id is not specified. Identifier of the
     /// sent message
     /// </param>
     /// <param name="ReplyMarkup">
     /// A JSON-serialized object for a new inline keyboard.
     /// </param>
     /// <returns>
     /// On success, if the message was sent by the bot, the sent Message is
     /// returned, otherwise True is returned.
     /// </returns>
    function stopMessageLiveLocation(ChatId: TValue; MessageId: Int64; ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
     /// <summary>
     /// Use this method to stop updating a live location message sent by the
     /// bot or via the bot (for inline bots) before live_period expires.
     /// </summary>
     /// <param name="InlineMessageId">
     /// Required if chat_id and message_id are not specified. Identifier of
     /// the inline message
     /// </param>
     /// <param name="ReplyMarkup">
     /// A JSON-serialized object for a new inline keyboard.
     /// </param>
     /// <returns>
     /// On success, if the message was sent by the bot, the sent Message is
     /// returned, otherwise True is returned.
     /// </returns>
    function stopMessageLiveLocation(const InlineMessageId: string; ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
     /// <summary>
     /// Use this method to edit only the reply markup of messages sent by the
     /// bot or via the bot (for inline bots).
     /// </summary>
     /// <param name="ChatId">
     /// Required if InlineMessageId is not specified. Unique identifier for <br />
     /// the target chat or username of the target channel (in the format <br />
     /// @channelusername) <br />
     /// </param>
     /// <param name="MessageId">
     /// Required if InlineMessageId is not specified. Unique identifier of <br />
     /// the sent message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// A JSON-serialized object for an inline keyboard. <br />
     /// </param>
     /// <returns>
     /// On success, if edited message is sent by the bot, the edited Message
     /// is returned, otherwise True is returned.
     /// </returns>
    function EditMessageReplyMarkup(ChatId: TValue; MessageId: Int64; ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
     /// <summary>
     /// Use this method to edit only the reply markup of messages sent by the
     /// bot or via the bot (for inline bots).
     /// </summary>
     /// <param name="InlineMessageId">
     /// Required if ChatId and MessageId are not specified. Identifier of <br />
     /// the inline message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// A JSON-serialized object for an inline keyboard. <br />
     /// </param>
     /// <returns>
     /// On success, if edited message is sent by the bot, the edited Message
     /// is returned, otherwise True is returned.
     /// </returns>
    function EditMessageReplyMarkup(const InlineMessageId: string; ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
     /// <summary>
     /// Use this method to delete a message.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="MessageId">
     /// Identifier of the message to delete <br />
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
     /// <remarks>
     /// A message can only be deleted if it was sent less than 48 hours ago.
     /// Any such recently sent outgoing message may be deleted. Additionally,
     /// if the bot is an administrator in a group chat, it can delete any
     /// message. If the bot is an administrator in a supergroup, it can
     /// delete messages from any other user and service messages about people
     /// joining or leaving the group (other types of service messages may
     /// only be removed by the group creator). In channels, bots can only
     /// remove their own messages.
     /// </remarks>
     /// <seealso href="https://core.telegram.org/bots/api#deletemessage" />
    function DeleteMessage(ChatId: TValue; MessageId: Int64): Boolean;
     {$ENDREGION}
{$REGION 'Inline mode'}
     /// <summary>
     /// Use this method to send answers to an inline query.
     /// </summary>
     /// <param name="InlineQueryId">
     /// Unique identifier for the answered query <br />
     /// </param>
     /// <param name="Results">
     /// A JSON-serialized array of results for the inline query <br />
     /// </param>
     /// <param name="CacheTime">
     /// The maximum amount of time in seconds that the result of the inline
     /// query may be cached on the server. Defaults to 300. <br />
     /// </param>
     /// <param name="IsPersonal">
     /// Pass True, if results may be cached on the server side only for the
     /// user that sent the query. By default, results may be returned to any
     /// user who sends the same query <br />
     /// </param>
     /// <param name="NextOffset">
     /// Pass the offset that a client should send in the next query with the
     /// same text to receive more results. Pass an empty string if there are
     /// no more results or if you don‘t support pagination. Offset length
     /// can’t exceed 64 bytes. <br />
     /// </param>
     /// <param name="SwitchPmText">
     /// If passed, clients will display a button with specified text that
     /// switches the user to a private chat with the bot and sends the bot a
     /// start message with the parameter switch_pm_parameter <br />
     /// </param>
     /// <param name="SwitchPmParameter">
     /// Parameter for the start message sent to the bot when user presses the
     /// switch button <br />
     /// </param>
     /// <returns>
     /// On success, True is returned.
     /// </returns>
     /// <remarks>
     /// No more than 50 results per query are allowed.
     /// </remarks>
     /// <seealso href="https://core.telegram.org/bots/api#answerinlinequery" />
    function AnswerInlineQuery(const InlineQueryId: string; Results: TArray<TtgInlineQueryResult>; CacheTime: Int64 = 300; IsPersonal: Boolean = False; const NextOffset: string = ''; const SwitchPmText: string = ''; const SwitchPmParameter: string = ''): Boolean;
{$ENDREGION}
{$REGION 'Payments'}
     /// <summary>
     /// Use this method to send invoices.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target private chat <br />
     /// </param>
     /// <param name="Title">
     /// Product name
     /// </param>
     /// <param name="Description">
     /// Product description
     /// </param>
     /// <param name="Payload">
     /// Bot-defined invoice payload, 1-128 bytes. This will not be displayed
     /// to the user, use for your internal processes.
     /// </param>
     /// <param name="ProviderToken">
     /// Payments provider token, obtained via Botfather
     /// </param>
     /// <param name="StartParameter">
     /// Unique deep-linking parameter that can be used to generate this
     /// invoice when used as a start parameter
     /// </param>
     /// <param name="Currency">
     /// Three-letter ISO 4217 currency code, see more on currencies
     /// </param>
     /// <param name="Prices">
     /// Price breakdown, a list of components (e.g. product price, tax,
     /// discount, delivery cost, delivery tax, bonus, etc.)
     /// </param>
     /// <param name="PhotoUrl">
     /// URL of the product photo for the invoice. Can be a photo of the goods
     /// or a marketing image for a service.
     /// </param>
     /// <param name="PhotoSize">
     /// Photo size
     /// </param>
     /// <param name="PhotoWidth">
     /// Photo width
     /// </param>
     /// <param name="PhotoHeight">
     /// Photo height
     /// </param>
     /// <param name="NeedName">
     /// Pass True, if you require the user's full name to complete the order
     /// </param>
     /// <param name="NeedPhoneNumber">
     /// Pass True, if you require the user's phone number to complete the
     /// order
     /// </param>
     /// <param name="NeedEmail">
     /// Pass True, if you require the user's email to complete the order
     /// </param>
     /// <param name="NeedShippingAddress">
     /// Pass True, if you require the user's shipping address to complete the
     /// order
     /// </param>
     /// <param name="IsFlexible">
     /// Pass True, if the final price depends on the shipping method
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound.
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// Additional interface options. A JSON-serialized object for a custom
     /// reply keyboard, instructions to hide keyboard or to force a reply
     /// from the user. <br />
     /// </param>
     /// <returns>
     /// On success, the sent <see cref="TelegAPi.Types|ITgMessage" /> is
     /// returned.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#sendinvoice" />
    function SendInvoice(ChatId: Int64; const title: string; const Description: string; const Payload: string; const ProviderToken: string; const StartParameter: string; const Currency: string; Prices: TArray<TtgLabeledPrice>; const ProviderData: string = ''; const PhotoUrl: string = ''; PhotoSize: Int64 = 0; PhotoWidth: Int64 = 0; PhotoHeight: Int64 = 0; NeedName: Boolean = False; NeedPhoneNumber: Boolean = False; NeedEmail: Boolean = False; NeedShippingAddress: Boolean = False; IsFlexible: Boolean = False; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
     /// <summary>
     /// If you sent an invoice requesting a shipping address and the
     /// parameter is_flexible was specified, the Bot API will send an Update
     /// with a shipping_query field to the bot. Use this method to reply to
     /// shipping queries. On success, True is returned.
     /// </summary>
     /// <param name="ShippingQueryId">
     /// Unique identifier for the query to be answered
     /// </param>
     /// <param name="Ok">
     /// Specify True if delivery to the specified address is possible and
     /// False if there are any problems (for example, if delivery to the
     /// specified address is not possible)
     /// </param>
     /// <param name="ShippingOptions">
     /// Required if <c>ok</c> is <c>True</c>. A JSON-serialized array of
     /// available shipping options.
     /// </param>
     /// <param name="ErrorMessage">
     /// Required if <c>ok</c> is <c>False</c>. Error message in human
     /// readable form that explains why it is impossible to complete the
     /// order (e.g. "Sorry, delivery to your desired address is
     /// unavailable'). Telegram will display this message to the user.
     /// </param>
     /// <seealso href="https://core.telegram.org/bots/api#answershippingquery" />
    function AnswerShippingQuery(const ShippingQueryId: string; Ok: Boolean; ShippingOptions: TArray<TtgShippingOption>; const ErrorMessage: string): Boolean;
     /// <summary>
     /// Once the user has confirmed their payment and shipping details, the
     /// Bot API sends the final confirmation in the form of an <see cref="TelegAPi.Types|TtgUpdate">
     /// Update</see> with the field PreCheckoutQueryId. Use this method to
     /// respond to such pre-checkout queries.
     /// </summary>
     /// <param name="PreCheckoutQueryId">
     /// Unique identifier for the query to be answered
     /// </param>
     /// <param name="Ok">
     /// Specify <c>True</c> if everything is alright (goods are available,
     /// etc.) and the bot is ready to proceed with the order. Use False if
     /// there are any problems.
     /// </param>
     /// <param name="ErrorMessage">
     /// Required if <c>ok</c> is <c>False</c>. Error message in human
     /// readable form that explains the reason for failure to proceed with
     /// the checkout (e.g. "Sorry, somebody just bought the last of our
     /// amazing black T-shirts while you were busy filling out your payment
     /// details. Please choose a different color or garment!"). Telegram will
     /// display this message to the user.
     /// </param>
     /// <returns>
     /// On success, True is returned.
     /// </returns>
     /// <remarks>
     /// <b>Note</b>: The Bot API must receive an answer within 10 seconds
     /// after the pre-checkout query was sent.
     /// </remarks>
     /// <seealso href="https://core.telegram.org/bots/api#answerprecheckoutquery" />
    function AnswerPreCheckoutQuery(const PreCheckoutQueryId: string; Ok: Boolean; const ErrorMessage: string = ''): Boolean;
{$ENDREGION}
{$REGION 'Games'}
     /// <summary>
     /// Use this method to send a game.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat <br />
     /// </param>
     /// <param name="GameShortName">
     /// Short name of the game, serves as the unique identifier for the game.
     /// Set up your games via Botfather. <br />
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound. <br />
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// A JSON-serialized object for an inline keyboard. If empty, one ‘Play
     /// game_title’ button will be shown. If not empty, the first button must
     /// launch the game. <br />
     /// </param>
     /// <returns>
     /// On success, the sent Message is returned.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#sendgame" />
    function SendGame(ChatId: Int64; const GameShortName: string; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
     /// <summary>
     /// Use this method to set the score of the specified user in a game.
     /// </summary>
     /// <param name="UserId">
     /// User identifier <br />
     /// </param>
     /// <param name="Score">
     /// New score, must be non-negative <br />
     /// </param>
     /// <param name="Force">
     /// Pass True, if the high score is allowed to decrease. This can be
     /// useful when fixing mistakes or banning cheaters <br />
     /// </param>
     /// <param name="DisableEditMessage">
     /// Pass True, if the game message should not be automatically edited to
     /// include the current scoreboard <br />
     /// </param>
     /// <param name="ChatId">
     /// Required if InlineMessageId is not specified. Unique identifier for <br />
     /// the target chat <br />
     /// </param>
     /// <param name="MessageId">
     /// Required if InlineMessageId is not specified. Identifier of the <br />
     /// sent message <br />
     /// </param>
     /// <param name="InlineMessageId">
     /// Required if ChatId and MessageId are not specified. Identifier of the
     /// inline message <br />
     /// </param>
     /// <returns>
     /// On success, if the message was sent by the bot, returns the edited
     /// Message, otherwise returns True. Returns an error, if the new score
     /// is not greater than the user's current score in the chat and force is
     /// False.
     /// </returns>
     /// <seealso href="https://core.telegram.org/bots/api#setgamescore" />
    function SetGameScore(UserId: Int64; Score: Int64; Force: Boolean = False; DisableEditMessage: Boolean = False; ChatId: Int64 = 0; MessageId: Int64 = 0; const InlineMessageId: string = ''): ITgMessage;
     /// <summary>
     /// Use this method to get data for high score tables. Will return the
     /// score of the specified user and several of his neighbors in a game.
     /// </summary>
     /// <param name="UserId">
     /// Target user id <br />
     /// </param>
     /// <param name="ChatId">
     /// Required if InlineMessageId is not specified. Unique identifier for <br />
     /// the target chat <br />
     /// </param>
     /// <param name="MessageId">
     /// Required if InlineMessageId is not specified. Identifier of the <br />
     /// sent message <br />
     /// </param>
     /// <param name="InlineMessageId">
     /// Required if ChatId and MessageId are not specified. Identifier of <br />
     /// the inline message <br />
     /// </param>
     /// <returns>
     /// On success, returns an Array of <see cref="TelegAPi.Types|TtgGameHighScore">
     /// GameHighScore</see> objects.
     /// </returns>
     /// <remarks>
     /// This method will currently return scores for the target user, plus
     /// two of his closest neighbors on each side. Will also return the top
     /// three users if the user and his neighbors are not among them. Please
     /// note that this behavior is subject to change.
     /// </remarks>
     /// <seealso href="https://core.telegram.org/bots/api#getgamehighscores">
     /// Official API
     /// </seealso>
    function GetGameHighScores(UserId: Int64; ChatId: Int64 = 0; MessageId: Int64 = 0; const InlineMessageId: string = ''): TArray<ItgGameHighScore>;
{$ENDREGION}
{$REGION 'Manage groups and channels'}
     /// <summary>
     /// Use this method to delete a chat photo.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format <c>@channelusername</c>)
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
     /// <remarks>
     /// Photos can't be changed for private chats. The bot must be an
     /// administrator in the chat for this to work and must have the
     /// appropriate admin rights.
     /// </remarks>
    function DeleteChatPhoto(ChatId: TValue): Boolean;
     /// <summary>
     /// Use this method to export an invite link to a supergroup or a
     /// channel.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format <c>@channelusername</c>)
     /// </param>
     /// <returns>
     /// Returns exported invite link as String on success.
     /// </returns>
     /// <remarks>
     /// The bot must be an administrator in the chat for this to work and
     /// must have the appropriate admin rights.
     /// </remarks>
    function ExportChatInviteLink(ChatId: TValue): string;
     /// <summary>
     /// Use this method to pin a message in a supergroup. The bot must be an
     /// administrator in the chat for this to work and must have the
     /// appropriate admin rights.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// supergroup (in the format <c>@supergroupusername</c>)
     /// </param>
     /// <param name="MessageId">
     /// Identifier of a message to pin <br />
     /// </param>
     /// <param name="DisableNotification">
     /// Pass True, if it is not necessary to send a notification to all group
     /// members about the new pinned message
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
    function PinChatMessage(ChatId: TValue; MessageId: Int64; DisableNotification: Boolean = False): Boolean;
     /// <summary>
     /// Use this method to change the description of a supergroup or a
     /// channel. The bot must be an administrator in the chat for this to
     /// work and must have the appropriate admin rights.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format <c>@channelusername</c>)
     /// </param>
     /// <param name="Description">
     /// New chat description, 0-255 characters
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
    function SetChatDescription(ChatId: TValue; const Description: string): Boolean;
     /// <summary>
     /// Use this method to set a new profile photo for the chat. Photos can't
     /// be changed for private chats.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format <c>@channelusername</c>)
     /// </param>
     /// <param name="Photo">
     /// New chat photo, uploaded using multipart/form-data
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
     /// <remarks>
     /// The bot must be an administrator in the chat for this to work and
     /// must have the appropriate admin rights.
     /// </remarks>
    function SetChatPhoto(ChatId: TValue; Photo: TtgFileToSend): Boolean;
     /// <summary>
     /// Use this method to change the title of a chat. Titles can't be
     /// changed for private chats. The bot must be an administrator in the
     /// chat for this to work and must have the appropriate admin rights.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format <c>@channelusername</c>)
     /// </param>
     /// <param name="Title">
     /// New chat title, 1-255 characters
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
     /// <remarks>
     /// Note: In regular groups (non-supergroups), this method will only work
     /// if the ‘All Members Are Admins’ setting is off in the target group.
     /// </remarks>
    function SetChatTitle(ChatId: TValue; const title: string): Boolean;
     /// <summary>
     /// Use this method to unpin a message in a supergroup chat. The bot must
     /// be an administrator in the chat for this to work and must have the
     /// appropriate admin rights.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// supergroup (in the format <c>@supergroupusername</c>)
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
    function UnpinChatMessage(ChatId: TValue): Boolean;
{$ENDREGION}
{$REGION 'Manage users and admins'}
     /// <summary>
     /// Use this method to restrict a user in a supergroup. The bot must be
     /// an administrator in the supergroup for this to work and must have the
     /// appropriate admin rights. Pass True for all boolean parameters to
     /// lift restrictions from a user.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// supergroup (in the format <c>@supergroupusername</c>)
     /// </param>
     /// <param name="UserId">
     /// Unique identifier of the target user
     /// </param>
     /// <param name="UntilDate">
     /// Date when restrictions will be lifted for the user, unix time. If
     /// user is restricted for more than 366 days or less than 30 seconds
     /// from the current time, they are considered to be restricted forever
     /// </param>
     /// <param name="CanSendMessages">
     /// Pass True, if the user can send text messages, contacts, locations
     /// and venues
     /// </param>
     /// <param name="CanSendMediaMessages">
     /// Pass True, if the user can send audios, documents, photos, videos,
     /// video notes and voice notes, implies CanSendMessages <br />
     /// </param>
     /// <param name="CanSendOtherMessages">
     /// Pass True, if the user can send animations, games, stickers and use
     /// inline bots, implies CanSendMediaMessages <br />
     /// </param>
     /// <param name="CanAddWebPagePreviews">
     /// Pass True, if the user may add web page previews to their messages,
     /// implies CanSendMediaMessages <br />
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
    function RestrictChatMember(ChatId: TValue; UserId: Int64; UntilDate: Int64 = 0; CanSendMessages: Boolean = False; CanSendMediaMessages: Boolean = False; CanSendOtherMessages: Boolean = False; CanAddWebPagePreviews: Boolean = False): Boolean;
     /// <summary>
     /// Use this method to restrict a user in a supergroup. The bot must be
     /// an administrator in the supergroup for this to work and must have the
     /// appropriate admin rights. Pass True for all boolean parameters to
     /// lift restrictions from a user.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// supergroup (in the format <c>@supergroupusername</c>)
     /// </param>
     /// <param name="UserId">
     /// Unique identifier of the target user
     /// </param>
     /// <param name="CanChangeInfo">
     /// Pass True, if the administrator can change chat title, photo and
     /// other settings
     /// </param>
     /// <param name="CanPostMessages">
     /// Pass True, if the administrator can create channel posts, channels
     /// only
     /// </param>
     /// <param name="CanEditMessages">
     /// Pass True, if the administrator can edit messages of other users,
     /// channels only
     /// </param>
     /// <param name="CanDeleteMessages">
     /// Pass True, if the administrator can delete messages of other users
     /// </param>
     /// <param name="CanInviteUsers">
     /// Pass True, if the administrator can invite new users to the chat
     /// </param>
     /// <param name="CanRestrictMembers">
     /// Pass True, if the administrator can restrict, ban or unban chat
     /// members
     /// </param>
     /// <param name="CanPinMessages">
     /// Pass True, if the administrator can pin messages, supergroups only
     /// </param>
     /// <param name="CanPromoteMembers">
     /// Pass True, if the administrator can add new administrators with a
     /// subset of his own privileges or demote administrators that he has
     /// promoted, directly or indirectly (promoted by administrators that
     /// were appointed by him)
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
    function PromoteChatMember(ChatId: TValue; UserId: Int64; CanChangeInfo: Boolean = False; CanPostMessages: Boolean = False; CanEditMessages: Boolean = False; CanDeleteMessages: Boolean = False; CanInviteUsers: Boolean = False; CanRestrictMembers: Boolean = False; CanPinMessages: Boolean = False; CanPromoteMembers: Boolean = False): Boolean;
{$ENDREGION}
{$REGION 'Strickers'}
     /// <summary>
     /// Use this method to send .webp stickers.
     /// </summary>
     /// <param name="ChatId">
     /// Unique identifier for the target chat or username of the target
     /// channel (in the format @channelusername) <br />
     /// </param>
     /// <param name="Sticker">
     /// Sticker to send. You can either pass a file_id as String to resend a
     /// sticker that is already on the Telegram servers, or upload a new
     /// sticker using multipart/form-data. <br />
     /// </param>
     /// <param name="DisableNotification">
     /// Sends the message silently. iOS users will not receive a
     /// notification, Android users will receive a notification with no
     /// sound. <br />
     /// </param>
     /// <param name="ReplyToMessageId">
     /// If the message is a reply, ID of the original message <br />
     /// </param>
     /// <param name="ReplyMarkup">
     /// Additional interface options. A JSON-serialized object for an inline
     /// keyboard, custom reply keyboard, instructions to hide reply keyboard
     /// or to force a reply from the user. <br />
     /// </param>
     /// <returns>
     /// On success, the sent Message is returned.
     /// </returns>
    function SendSticker(ChatId: TValue; Sticker: TValue; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
     /// <summary>
     /// Use this method to get a sticker set.
     /// </summary>
     /// <param name="name">
     /// Name of the sticker set
     /// </param>
     /// <returns>
     /// On success, a <see cref="TelegAPi.Types|TtgStickerSet">StickerSet</see>
     /// object is returned.
     /// </returns>
    function getStickerSet(const Name: string): TtgStickerSet;
     /// <summary>
     /// Use this method to upload a .png file with a sticker for later use in
     /// <see cref="TelegAPI.Bot|TTelegramBot.createNewStickerSet(Int64,string,string,TValue,string,Boolean,TtgMaskPosition)">
     /// createNewStickerSet</see> and <see cref="TelegAPI.Bot|TTelegramBot.addStickerToSet(Int64,string,TValue,string,TtgMaskPosition)">
     /// addStickerToSet</see> methods (can be used multiple times). <br />
     /// </summary>
     /// <param name="UserId">
     /// User identifier of sticker file owner <br />
     /// </param>
     /// <param name="PngSticker">
     /// Png image with the sticker, must be up to 512 kilobytes in size,
     /// dimensions must not exceed 512px, and either width or height must be
     /// exactly 512px. <br />
     /// </param>
     /// <returns>
     /// Returns the uploaded <see cref="TelegAPi.Types|TtgFile">File</see> on
     /// success.
     /// </returns>
    function uploadStickerFile(UserId: Int64; PngSticker: TtgFileToSend): ItgFile;
     /// <summary>
     /// Use this method to create new sticker set owned by a user. The bot
     /// will be able to edit the created sticker set.
     /// </summary>
     /// <param name="UserId">
     /// User identifier of created sticker set owner <br />
     /// </param>
     /// <param name="Name">
     /// Short name of sticker set, to be used in t.me/addstickers/ URLs
     /// (e.g., animals). Can contain only english letters, digits and
     /// underscores. Must begin with a letter, can't contain consecutive
     /// underscores and must end in by “__&lt;bot username&gt;”.
     /// &lt;bot_username&gt; is case insensitive. 1-64 characters. <br />
     /// </param>
     /// <param name="Title">
     /// Sticker set title, 1-64 characters <br />
     /// </param>
     /// <param name="PngSticker">
     /// Png image with the sticker, must be up to 512 kilobytes in size,
     /// dimensions must not exceed 512px, and either width or height must be
     /// exactly 512px. Pass a file_id as a String to send a file that already
     /// exists on the Telegram servers, pass an HTTP URL as a String for
     /// Telegram to get a file from the Internet, or upload a new one using
     /// multipart/form-data. More info on Sending Files » <br />
     /// </param>
     /// <param name="Emojis">
     /// One or more emoji corresponding to the sticker <br />
     /// </param>
     /// <param name="ContainsMasks">
     /// Pass True, if a set of mask stickers should be created <br />
     /// </param>
     /// <param name="MaskPosition">
     /// A JSON-serialized object for position where the mask should be placed
     /// on faces <br />
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
    function createNewStickerSet(UserId: Int64; const Name, title: string; PngSticker: TValue; const Emojis: string; ContainsMasks: Boolean = False; MaskPosition: TtgMaskPosition = nil): Boolean;
     /// <summary>
     /// Use this method to add a new sticker to a set created by the bot.
     /// </summary>
     /// <returns>
     /// Returns True on success.
     /// </returns>
    function addStickerToSet(UserId: Int64; const Name: string; PngSticker: TValue; const Emojis: string; MaskPosition: TtgMaskPosition = nil): Boolean;
     /// <summary>
     /// Use this method to move a sticker in a set created by the bot to a
     /// specific position.
     /// </summary>
     /// <param name="Sticker">
     /// File identifier of the sticker
     /// </param>
     /// <param name="Position">
     /// New sticker position in the set, zero-based
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
    function setStickerPositionInSet(const Sticker: string; Position: Int64): Boolean;
     /// <summary>
     /// Use this method to delete a sticker from a set created by the bot.
     /// </summary>
     /// <param name="Sticker">
     /// File identifier of the sticker
     /// </param>
     /// <returns>
     /// Returns True on success.
     /// </returns>
    function deleteStickerFromSet(const Sticker: string): Boolean;
     /// <summary>
     /// Use this method to set a new group sticker set for a supergroup.
     /// </summary>
     /// <returns>
     /// Returns True on success.
     /// </returns>
     /// <remarks>
     /// The bot must be an administrator in the chat for this to work and
     /// must have the appropriate admin rights. Use the field <see cref="TelegAPi.Types|TtgChat.CanSetStickerSet">
     /// CanSetStickerSet</see> optionally returned in <see cref="TelegAPI.Bot|TTelegramBot.GetChat(TValue)">
     /// getChat</see> requests to check if the bot can use this method.
     /// </remarks>
    function setChatStickerSet(ChatId: TValue; const StickerSetName: string): Boolean;
     /// <summary>
     /// Use this method to delete a group sticker set from a supergroup.
     /// </summary>
     /// <returns>
     /// Returns True on success.
     /// </returns>
     /// <remarks>
     /// The bot must be an administrator in the chat for this to work and
     /// must have the appropriate admin rights. Use the field <see cref="TelegAPi.Types|TtgChat.CanSetStickerSet">
     /// CanSetStickerSet</see> optionally returned in <see cref="TelegAPI.Bot|TTelegramBot.GetChat(TValue)">
     /// getChat</see> requests to check if the bot can use this method.
     /// </remarks>
    function deleteChatStickerSet(ChatId: TValue): Boolean;
    function sendMediaGroup(ChatId: TValue; AMedia: TArray<TtgInputMedia>; ADisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0): TArray<ITgMessage>;
{$ENDREGION}
    property Token: string read GetToken write SetToken;
    property ExceptionManager: ItgExceptionHandler read GetExceptionManager write SetExceptionManager;
  end;

implementation

end.

