unit TelegAPI.Bot;

interface

{ } {$DEFINE TG_AUTOLib}
{ }   {$IF Defined(TG_AUTOLib)}
{ }   {$I jedi.inc}
{ }   {$IF Defined(DELPHIXE8_UP)}
{ }     {$DEFINE TG_NetHttpClient}
{ }   {$ELSE}
{ }     {$DEFINE TG_INDY}
{ }   {$ENDIF}
{ } {$ENDIF}

uses
  System.Generics.Collections,
  System.Rtti,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
{$IF Defined(TG_NetHttpClient)}
  System.Net.Mime,
  System.Net.HttpClient,
{$ELSEIF Defined(TG_INDY)}
  IdMultiPartFormData,
  IdHttp,
{$ENDIF}
  TelegAPI.Classes,
  TelegAPI.Utils,
  XSuperObject;

Type
{$IF Defined(TG_NetHttpClient)}
{$ELSEIF Defined(TG_INDY)}
  TMultipartFormData = TIdMultiPartFormDataStream;

  TMultipartFormDataHelper = Class Helper for TIdMultiPartFormDataStream
    procedure AddField(const AField, AValue: string);
  End;
{$ENDIF}

  TtgBotOnUpdates = procedure(Sender: TObject; Updates: TArray<TtgUpdate>) of Object;
  TtgBorOnError = procedure(Sender: TObject; Const Code: Integer; Const Message: String) of Object;

type
  TTelegramBot = class;

  TtgRecesiver = Class(TThread)
  private
    fBot: TTelegramBot;
  protected
    procedure Execute; override;
  public
    property Bot: TTelegramBot read fBot write fBot;
  End;

  TTelegramBot = Class(TComponent)
  private
    FToken: String;
    FOnUpdates: TtgBotOnUpdates;
    FUploadTimeout: Integer;
    FPollingTimeout: Integer;
    FMessageOffset: Integer;
    FOnError: TtgBorOnError;
    FRecesiver: TtgRecesiver;
    fIsReceiving: Boolean;
    fAllowedUpdates: TAllowedUpdates;
    procedure SetIsReceiving(const Value: Boolean);
    function GetVersionAPI: String;
  protected
    /// <summary>Мастер-функция для запросов на сервак</summary>
    Function API<T>(Const Method: String; Parameters: TDictionary<String, TValue>): T;
    Function ParamsToFormData(Parameters: TDictionary<String, TValue>): TMultipartFormData;
    Function AllowedUpdatesToString(allowed_updates: TAllowedUpdates): String;
  public
    /// <summary>A simple method for testing your bot's auth token.</summary>
    /// <returns>Returns basic information about the bot in form of a User object.</returns>
    Function getMe: TtgUser;
    /// <summary>Use this method to receive incoming updates using long polling. An Array of Update objects is returned.</summary>
    /// <param name="offset">Identifier of the first update to be returned. Must be greater by one than the highest among the identifiers of previously received updates. By default, updates starting with the earliest unconfirmed update are returned. An update is considered confirmed as soon as getUpdates is called with an offset higher than its update_id. The negative offset can be specified to retrieve updates starting from -offset update from the end of the updates queue. All previous updates will forgotten. </param>
    /// <param name="limit">Limits the number of updates to be retrieved. Values between 1—100 are accepted. Defaults to 100. </param>
    /// <param name="timeout">Timeout in seconds for long polling. Defaults to 0, i.e. usual short polling</param>
    /// <remarks>1. This method will not work if an outgoing webhook is set up. 2. In order to avoid getting duplicate updates, recalculate offset after each server response.</remarks>
    Function getUpdates(Const offset: Integer = 0; Const limit: Integer = 100;
      Const timeout: Integer = 0; allowed_updates: TAllowedUpdates = UPDATES_ALLOWED_ALL)
      : TArray<TtgUpdate>;
    /// <summary>
    /// Use this method to specify a url and receive incoming updates via an
    /// outgoing webhook. Whenever there is an update for the bot, we will
    /// send an HTTPS POST request to the specified url, containing a
    /// JSON-serialized Update. In case of an unsuccessful request, we will
    /// give up after a reasonable amount of attempts.
    /// </summary>
    /// <param name="url">
    /// HTTPS url to send updates to. Use an empty string to remove webhook
    /// integration
    /// </param>
    /// <param name="certificate">
    /// Upload your public key certificate so that the root certificate in
    /// use can be checked. See our self-signed guide for details.
    /// </param>
    /// <param name="max_connections">
    /// Maximum allowed number of simultaneous HTTPS connections to the
    /// webhook for update delivery, 1-100. Defaults to 40. Use lower values
    /// to limit the load on your bot‘s server, and higher values to increase
    /// your bot’s throughput.
    /// </param>
    /// <remarks>
    /// <para>
    /// Notes
    /// </para>
    /// <para>
    /// 1. You will not be able to receive updates using getUpdates for
    /// as long as an outgoing webhook is set up.
    /// </para>
    /// <para>
    /// 2. To use a self-signed certificate, you need to upload your
    /// public key certificate using certificate parameter. Please upload
    /// as InputFile, sending a String will not work.
    /// </para>
    /// <para>
    /// 3. Ports currently supported for Webhooks: 443, 80, 88, 8443.
    /// </para>
    /// </remarks>
    Procedure setWebhook(const url: String; certificate: TtgFileToSend = nil;
      max_connections: Integer = 40; allowed_updates: TAllowedUpdates = UPDATES_ALLOWED_ALL);
    /// <summary>
    /// Use this method to remove webhook integration if you decide to switch
    /// back to getUpdates.
    /// </summary>
    /// <returns>
    /// Returns True on success.
    /// </returns>
    /// <seealso cref="TelegAPI.Bot|TTelegramBot.setWebhook(string,TtgFileToSend,TAllowedUpdates)">
    /// getUpdates
    /// </seealso>
    Function deleteWebhook: Boolean;
    /// <summary>
    /// Use this method to get current webhook status. Requires no
    /// parameters.
    /// </summary>
    /// <returns>
    /// On success, returns a WebhookInfo object
    /// </returns>
    /// <remarks>
    /// If the bot is using getUpdates, will return an object with the url
    /// field empty
    /// </remarks>
    Function getWebhookInfo: TtgWebhookInfo;
    /// <summary>Use this method to send text messages.</summary>
    /// <param name="chat_id">Integer or String. Unique identifier for the target chat or username of the target channel (in the format @channelusername).</param>
    /// <param name="text">Text of the message to be sent</param>
    /// <param name="parse_mode">Send Markdown or HTML, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.</param>
    /// <param name="disable_web_page_preview">Disables link previews for links in this message</param>
    /// <param name="disable_notification">Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.</param>
    /// <param name="reply_to_message_id">If the message is a reply, ID of the original message</param>
    /// <param name="reply_markup">InlineKeyboardMarkup or ReplyKeyboardMarkup or ReplyKeyboardHide or ForceReply. Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to hide reply keyboard or to force a reply from the user.</param>
    /// <returns>On success, the sent Message is returned.</returns>
    Function sendMessage(Const chat_id: TValue; const text: String;
      ParseMode: TtgParseMode = TtgParseMode.Default; disableWebPagePreview: Boolean = False;
      disable_notification: Boolean = False; replyToMessageId: Integer = 0;
      replyMarkup: TtgReplyMarkup = nil): TtgMessage;
    /// <summary>Use this method to forward messages of any kind.</summary>
    /// <returns>On success, the sent Message is returned.</returns>
    /// <param name="chat_id">Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="from_chat_id">Unique identifier for the chat where the original message was sent (or channel username in the format @channelusername)</param>
    /// <param name="disable_notification">Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.</param>
    /// <param name="message_id">Unique message identifier</param>
    Function forwardMessage(chat_id: TValue; from_chat_id: TValue;
      disable_notification: Boolean = False; message_id: Integer = 0): TtgMessage;
    /// <summary>
    /// Use this method to send photos.
    /// </summary>
    /// <param name="chatId">
    /// Unique identifier for the target chat or username of the target
    /// channel (in the format @channelusername)
    /// </param>
    /// <param name="photo">
    /// Photo to send. You can either pass a file_id as String to resend a
    /// photo that is already on the Telegram servers, or upload a new photo
    /// using multipart/form-data.
    /// </param>
    /// <param name="caption">
    /// Photo caption (may also be used when resending photos by file_id),
    /// 0-200 characters
    /// </param>
    /// <param name="disable_notification">
    /// Sends the message silently. iOS users will not receive a
    /// notification, Android users will receive a notification with no
    /// sound.
    /// </param>
    /// <param name="replyToMessageId">
    /// If the message is a reply, ID of the original message
    /// </param>
    /// <param name="replyMarkup">
    /// Additional interface options. A JSON-serialized object for an inline
    /// keyboard, custom reply keyboard, instructions to remove reply
    /// keyboard or to force a reply from the user.
    /// </param>
    /// <returns>
    /// On success, the sent Message is returned.
    /// </returns>
    /// <example>
    /// var <br />LMessage: TtgMessage; <br />Begin <br />//Если не известен
    /// ИД файла <br />LMessage := sendPhoto(chatId,
    /// TtgFileToSend.Create('Путь к файлу'), nil); <br />//Если известен ИД
    /// файла <br />LMessage := sendPhoto(chatId, 'ИД Файла'); <br />... <br />
    /// LMessage.Free; <br />End;
    /// </example>
    Function sendPhoto(chatId: TValue; photo: TValue; Const caption: string = '';
      disable_notification: Boolean = False; replyToMessageId: Integer = 0;
      replyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>Use this method to send audio files, if you want Telegram clients to display them in the music player. Your audio must be in the .mp3 format.</summary>
    /// <returns>On success, the sent Message is returned.</returns>
    /// <remarks>Bots can currently send audio files of up to 50 MB in size, this limit may be changed in the future. For sending voice messages, use the sendVoice method instead.</remarks>
    /// <param name="chat_id">Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="audio">Audio file to send. You can either pass a file_id as String to resend an audio that is already on the Telegram servers, or upload a new audio file using multipart/form-data.</param>
    /// <param name="duration">Duration of the audio in seconds</param>
    /// <param name="performer">Performer</param>
    /// <param name="title">Track name</param>
    /// <param name="disable_notification">Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.</param>
    /// <param name="reply_to_message_id">If the message is a reply, ID of the original message</param>
    /// <param name="reply_markup">Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to hide reply keyboard or to force a reply from the user.</param>
    Function sendAudio(chat_id: TValue; audio: TValue; duration: Integer = 0;
      Const performer: String = ''; Const title: String = ''; disable_notification: Boolean = False;
      reply_to_message_id: Integer = 0; replyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>Use this method to send general files.</summary>
    /// <returns>On success, the sent Message is returned. </returns>
    /// <remarks>Bots can currently send files of any type of up to 50 MB in size, this limit may be changed in the future.</remarks>
    /// <param name="chat_id">Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="document">File to send. You can either pass a file_id as String to resend a file that is already on the Telegram servers, or upload a new file using multipart/form-data.</param>
    /// <param name="caption">Document caption (may also be used when resending documents by file_id), 0-200 characters</param>
    /// <param name="disable_notification">Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.</param>
    /// <param name="reply_to_message_id">If the message is a reply, ID of the original message</param>
    /// <param name="reply_markup">Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to hide reply keyboard or to force a reply from the user.</param>
    Function sendDocument(chat_id: TValue; document: TValue; Const caption: String = '';
      disable_notification: Boolean = False; reply_to_message_id: Integer = 0;
      reply_markup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>Use this method to send .webp stickers.</summary>
    /// <returns>On success, the sent Message is returned.</returns>
    /// <remarks> </remarks>
    /// <param name="chat_id">Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="sticker">Sticker to send. You can either pass a file_id as String to resend a sticker that is already on the Telegram servers, or upload a new sticker using multipart/form-data.</param>
    /// <param name="disable_notification">Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.</param>
    /// <param name="reply_to_message_id">If the message is a reply, ID of the original message</param>
    /// <param name="reply_markup">Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to hide reply keyboard or to force a reply from the user.</param>
    Function sendSticker(chat_id: TValue; sticker: TValue; Const caption: String = '';
      disable_notification: Boolean = False; reply_to_message_id: Integer = 0;
      reply_markup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>Use this method to send video files, Telegram clients support mp4 videos (other formats may be sent as Document). </summary>
    /// <returns>On success, the sent Message is returned.</returns>
    /// <remarks>Bots can currently send video files of up to 50 MB in size, this limit may be changed in the future.</remarks>
    /// <param name="chat_id">Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="video">Video to send. You can either pass a file_id as String to resend a video that is already on the Telegram servers, or upload a new video file using multipart/form-data.</param>
    /// <param name="duration">Duration of sent video in seconds</param>
    /// <param name="width">Video width</param>
    /// <param name="height">Video height</param>
    /// <param name="caption">Video caption (may also be used when resending videos by file_id), 0-200 characters</param>
    /// <param name="disable_notification">Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.</param>
    /// <param name="reply_to_message_id">If the message is a reply, ID of the original message</param>
    /// <param name="reply_markup">Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to hide reply keyboard or to force a reply from the user.</param>
    function sendVideo(chat_id: TValue; video: TValue; duration: Integer = 0; width: Integer = 0;
      height: Integer = 0; const caption: String = ''; disable_notification: Boolean = False;
      reply_to_message_id: Integer = 0; reply_markup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>Use this method to send audio files, if you want Telegram clients to display the file as a playable voice message. For this to work, your audio must be in an .ogg file encoded with OPUS (other formats may be sent as Audio or Document).</summary>
    /// <returns>On success, the sent Message is returned</returns>
    /// <remarks>Bots can currently send voice messages of up to 50 MB in size, this limit may be changed in the future.</remarks>
    /// <param name="chat_id">Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="voice">Audio file to send. You can either pass a file_id as String to resend an audio that is already on the Telegram servers, or upload a new audio file using multipart/form-data.</param>
    /// <param name="duration">Duration of sent audio in seconds</param>
    /// <param name="disable_notification">Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.</param>
    /// <param name="reply_to_message_id">If the message is a reply, ID of the original message</param>
    /// <param name="reply_markup">Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to hide reply keyboard or to force a reply from the user.</param>
    Function sendVoice(chat_id: TValue; voice: TValue; duration: Integer = 0;
      disable_notification: Boolean = False; reply_to_message_id: Integer = 0;
      reply_markup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>Use this method to send point on the map.</summary>
    /// <returns>On success, the sent Message is returned.</returns>
    /// <remarks> </remarks>
    /// <param name="chat_id">Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="latitude">Latitude of location</param>
    /// <param name="longitude">Longitude of location</param>
    /// <param name="disable_notification">Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.</param>
    /// <param name="reply_to_message_id">If the message is a reply, ID of the original message</param>
    /// <param name="reply_markup">Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to hide reply keyboard or to force a reply from the user.</param>
    /// <param name=""></param>
    Function sendLocation(chat_id: TValue; Location: TtgLocation;
      disable_notification: Boolean = False; reply_to_message_id: Integer = 0;
      reply_markup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>Use this method to send information about a venue.</summary>
    /// <returns>On success, the sent Message is returned.</returns>
    /// <remarks> </remarks>
    /// <param name="chat_id">Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="latitude">Latitude of the venue</param>
    /// <param name="longitude">Longitude of the venue</param>
    /// <param name="title">Name of the venue</param>
    /// <param name="address">Address of the venue</param>
    /// <param name="foursquare_id">Foursquare identifier of the venue</param>
    /// <param name="disable_notification">Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.</param>
    /// <param name="reply_to_message_id">If the message is a reply, ID of the original message</param>
    /// <param name="reply_markup">Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to hide reply keyboard or to force a reply from the user.</param>
    Function sendVenue(chat_id: TValue; venue: TtgVenue; disable_notification: Boolean = False;
      reply_to_message_id: Integer = 0; reply_markup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>Use this method to send phone contacts.</summary>
    /// <returns>On success, the sent Message is returned.</returns>
    /// <param name="chat_id">Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="phone_number">Contact's phone number</param>
    /// <param name="first_name">Contact's first name</param>
    /// <param name="last_name">Contact's last name</param>
    /// <param name="disable_notification">Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.</param>
    /// <param name="reply_to_message_id">If the message is a reply, ID of the original message</param>
    /// <param name="reply_markup">Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to hide keyboard or to force a reply from the user.</param>
    Function sendContact(chat_id: TValue; contact: TtgContact;
      disable_notification: Boolean = False; reply_to_message_id: Integer = 0;
      reply_markup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>Use this method when you need to tell the user that something is happening on the bot's side. The status is set for 5 seconds or less (when a message arrives from your bot, Telegram clients clear its typing status).</summary>
    /// <remarks>We only recommend using this method when a response from the bot will take a noticeable amount of time to arrive.</remarks>
    /// <param name="chat_id">Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="action">Type of action to broadcast. Choose one, depending on what the user is about to receive: typing for text messages, upload_photo for photos, record_video or upload_video for videos, record_audio or upload_audio for audio files, upload_document for general files, find_location for location data</param>
    Procedure sendChatAction(chat_id: TValue; Const action: String);
    /// <summary>
    /// Use this method to get a list of profile pictures for a user.
    /// </summary>
    /// <param name="chat_id">
    /// Unique identifier of the target user <br />
    /// </param>
    /// <param name="offset">
    /// Sequential number of the first photo to be returned. By default, all
    /// photos are returned.
    /// </param>
    /// <param name="limit">
    /// Limits the number of photos to be retrieved. Values between 1—100 are
    /// accepted. Defaults to 100.
    /// </param>
    /// <returns>
    /// Returns a UserProfilePhotos object.
    /// </returns>
    Function getUserProfilePhotos(chat_id: TValue; offset: Integer; limit: Integer = 100)
      : TtgUserProfilePhotos;
    /// <summary>Use this method to get basic info about a file and prepare it for downloading. For the moment, bots can download files of up to 20MB in size.</summary>
    /// <returns>On success, a File object is returned.</returns>
    /// <param name="file_id">File identifier to get info about</param>
    Function getFile(Const file_id: String): TtgFile;
    /// <summary>Use this method to kick a user from a group or a supergroup. In the case of supergroups, the user will not be able to return to the group on their own using invite links, etc., unless unbanned first. The bot must be an administrator in the group for this to work.</summary>
    /// <returns>Returns True on success.</returns>
    /// <remarks>Note: This will method only work if the ‘All Members Are Admins’ setting is off in the target group. Otherwise members may only be removed by the group's creator or by the member that added them.</remarks>
    /// <param name="chat_id">Unique identifier for the target group or username of the target supergroup (in the format @supergroupusername)</param>
    /// <param name="user_id">Unique identifier of the target user</param>
    Function kickChatMember(chat_id: TValue; user_id: Integer): Boolean;
    /// <summary>Use this method for your bot to leave a group, supergroup or channel.</summary>
    /// <param name="chat_id">Unique identifier for the target group or username of the target supergroup (in the format @supergroupusername)</param>
    /// <returns>Returns True on success.</returns>
    function leaveChat(chat_id: TValue): Boolean;
    /// <summary>Use this method to unban a previously kicked user in a supergroup. The user will not return to the group automatically, but will be able to join via link, etc. The bot must be an administrator in the group for this to work.</summary>
    /// <returns>Returns True on success.</returns>
    /// <remarks> </remarks>
    /// <param name="chat_id">Unique identifier for the target group or username of the target supergroup (in the format @supergroupusername)</param>
    /// <param name="user_id">Unique identifier of the target user</param>
    Function unbanChatMember(chat_id: TValue; user_id: Integer): Boolean;
    /// <summary>Use this method to get up to date information about the chat (current name of the user for one-on-one conversations, current username of a user, group or channel, etc.)</summary>
    /// <param name="chat_id">Unique identifier for the target chat or username of the target supergroup or channel (in the format @channelusername)</param>
    /// <returns>Returns a Chat object on success.</returns>
    function getChat(Const chat_id: TValue): TtgChat;
    /// <summary>
    /// Use this method to get a list of administrators in a chat
    /// </summary>
    /// <param name="chat_id">
    /// Unique identifier for the target chat or username of the target
    /// supergroup or channel (in the format @channelusername)
    /// </param>
    /// <returns>
    /// On success, returns an Array of ChatMember objects that contains
    /// information about all chat administrators except other bots. If the
    /// chat is a group or a supergroup and no administrators were appointed,
    /// only the creator will be returned.
    /// </returns>
    function getChatAdministrators(Const chat_id: TValue): TArray<TtgChatMember>;
    /// <summary>
    /// Use this method to get the number of members in a chat.
    /// </summary>
    /// <param name="chat_id">
    /// Unique identifier for the target chat or username of the target
    /// supergroup or channel (in the format @channelusername)
    /// </param>
    /// <returns>
    /// Returns Int on success.
    /// </returns>
    function getChatMembersCount(Const chat_id: TValue): Integer;
    /// <summary>Use this method to get information about a member of a chat.</summary>
    /// <returns>Returns a ChatMember object on success.</returns>
    /// <param name="chat_id">Unique identifier for the target group or username of the target supergroup (in the format @supergroupusername)</param>
    /// <param name="user_id">Unique identifier of the target user</param>
    function getChatMember(chat_id: TValue; user_id: Integer): TtgChatMember;
    /// <summary>Use this method to send answers to callback queries sent from inline keyboards. The answer will be displayed to the user as a notification at the top of the chat screen or as an alert.</summary>
    /// <returns>On success, True is returned.</returns>
    /// <remarks> </remarks>
    /// <param name="callback_query_id">Unique identifier for the query to be answered</param>
    /// <param name="text">Text of the notification. If not specified, nothing will be shown to the user</param>
    /// <param name="show_alert">If true, an alert will be shown by the client instead of a notification at the top of the chat screen. Defaults to false.</param>
    Function answerCallbackQuery(Const callback_query_id: String; Const text: String = '';
      show_alert: Boolean = False): Boolean;
    /// <summary>Use this method to edit text messages sent by the bot or via the bot (for inline bots).</summary>
    /// <returns>On success, if edited message is sent by the bot, the edited Message is returned, otherwise True is returned.</returns>
    /// <remarks> </remarks>
    /// <param name="chat_id">Required if inline_message_id is not specified. Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="message_id">Required if inline_message_id is not specified. Unique identifier of the sent message</param>
    /// <param name="inline_message_id">Required if chat_id and message_id are not specified. Identifier of the inline message</param>
    /// <param name="text">New text of the message</param>
    /// <param name="parse_mode">Send Markdown or HTML, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.</param>
    /// <param name="disable_web_page_preview">Disables link previews for links in this message</param>
    /// <param name="reply_markup">A JSON-serialized object for an inline keyboard.</param>
    Function editMessageText(chat_id: TValue; message_id: Integer; Const inline_message_id: String;
      Const text: String; parse_mode: TtgParseMode = TtgParseMode.Default;
      disable_web_page_preview: Boolean = False;
      reply_markup: TtgReplyKeyboardMarkup = nil): Boolean;
    /// <summary>Use this method to edit captions of messages sent by the bot or via the bot (for inline bots). </summary>
    /// <returns>On success, if edited message is sent by the bot, the edited Message is returned, otherwise True is returned.</returns>
    /// <remarks> </remarks>
    /// <param name="chat_id">Required if inline_message_id is not specified. Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="message_id">Required if inline_message_id is not specified. Unique identifier of the sent message</param>
    /// <param name="inline_message_id">Required if chat_id and message_id are not specified. Identifier of the inline message</param>
    /// <param name="caption">New caption of the message</param>
    /// <param name="reply_markup">A JSON-serialized object for an inline keyboard.</param>
    Function editMessageCaption(chat_id: TValue; message_id: Integer;
      Const inline_message_id: String; Const caption: String;
      reply_markup: TtgReplyKeyboardMarkup = nil): Boolean;
    /// <summary>Use this method to edit only the reply markup of messages sent by the bot or via the bot (for inline bots).</summary>
    /// <returns>On success, if edited message is sent by the bot, the edited Message is returned, otherwise True is returned.</returns>
    /// <remarks> </remarks>
    /// <param name="chat_id">Required if inline_message_id is not specified. Unique identifier for the target chat or username of the target channel (in the format @channelusername)</param>
    /// <param name="message_id">Required if inline_message_id is not specified. Unique identifier of the sent message</param>
    /// <param name="inline_message_id">Required if chat_id and message_id are not specified. Identifier of the inline message</param>
    /// <param name="reply_markup">A JSON-serialized object for an inline keyboard.</param>
    Function editMessageReplyMarkup(chat_id: TValue; message_id: Integer;
      Const inline_message_id: String; reply_markup: TtgReplyKeyboardMarkup = nil): Boolean;
    /// <summary>Use this method to send answers to an inline query.</summary>
    /// <returns>On success, True is returned.</returns>
    /// <remarks>No more than 50 results per query are allowed.</remarks>
    /// <param name="inline_query_id">Unique identifier for the answered query</param>
    /// <param name="results">A JSON-serialized array of results for the inline query</param>
    /// <param name="cache_time">The maximum amount of time in seconds that the result of the inline query may be cached on the server. Defaults to 300.</param>
    /// <param name="is_personal">Pass True, if results may be cached on the server side only for the user that sent the query. By default, results may be returned to any user who sends the same query</param>
    /// <param name="next_offset">Pass the offset that a client should send in the next query with the same text to receive more results. Pass an empty string if there are no more results or if you don‘t support pagination. Offset length can’t exceed 64 bytes.</param>
    /// <param name="switch_pm_text">If passed, clients will display a button with specified text that switches the user to a private chat with the bot and sends the bot a start message with the parameter switch_pm_parameter</param>
    /// <param name="switch_pm_parameter">Parameter for the start message sent to the bot when user presses the switch button</param>
    Function answerInlineQuery(Const inline_query_id: String; results: TArray<TtgInlineQueryResult>;
      cache_time: Integer = 300; is_personal: Boolean = False; Const next_offset: String = '';
      Const switch_pm_text: String = ''; Const switch_pm_parameter: String = ''): Boolean;
    /// <summary>
    /// Use this method to send a game.
    /// </summary>
    /// <param name="chat_id">
    /// Unique identifier for the target chat
    /// </param>
    /// <param name="game_short_name">
    /// Short name of the game, serves as the unique identifier for the game.
    /// Set up your games via Botfather.
    /// </param>
    /// <param name="disable_notification">
    /// Sends the message silently. iOS users will not receive a
    /// notification, Android users will receive a notification with no
    /// sound.
    /// </param>
    /// <param name="reply_to_message_id">
    /// If the message is a reply, ID of the original message
    /// </param>
    /// <param name="reply_markup">
    /// A JSON-serialized object for an inline keyboard. If empty, one ‘Play
    /// game_title’ button will be shown. If not empty, the first button must
    /// launch the game.
    /// </param>
    /// <returns>
    /// On success, the sent Message is returned.
    /// </returns>
    Function sendGame(chat_id: Integer; Const game_short_name: String;
      disable_notification: Boolean = False; reply_to_message_id: Integer = 0;
      reply_markup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>
    /// Use this method to set the score of the specified user in a game.
    /// </summary>
    /// <param name="user_id">
    /// User identifier
    /// </param>
    /// <param name="score">
    /// New score, must be non-negative
    /// </param>
    /// <param name="force">
    /// Pass True, if the high score is allowed to decrease. This can be
    /// useful when fixing mistakes or banning cheaters
    /// </param>
    /// <param name="disable_edit_message">
    /// Pass True, if the game message should not be automatically edited to
    /// include the current scoreboard
    /// </param>
    /// <param name="chat_id">
    /// Required if inline_message_id is not specified. Unique identifier for
    /// the target chat
    /// </param>
    /// <param name="message_id">
    /// Required if inline_message_id is not specified. Identifier of the
    /// sent message
    /// </param>
    /// <param name="inline_message_id">
    /// Required if chat_id and message_id are not specified. Identifier of
    /// the inline message
    /// </param>
    /// <returns>
    /// On success, if the message was sent by the bot, returns the edited
    /// Message, otherwise returns True. Returns an error, if the new score
    /// is not greater than the user's current score in the chat and force is
    /// False.
    /// </returns>
    Function setGameScore(user_id: Integer; score: Integer; force: Boolean = False;
      disable_edit_message: Boolean = False; chat_id: Integer = 0; message_id: Integer = 0;
      Const inline_message_id: String = ''): TtgMessage;
    /// <summary>
    /// Use this method to get data for high score tables. Will return the
    /// score of the specified user and several of his neighbors in a game.
    /// </summary>
    /// <param name="user_id">
    /// Target user id
    /// </param>
    /// <param name="chat_id">
    /// Required if inline_message_id is not specified. Unique identifier for
    /// the target chat
    /// </param>
    /// <param name="message_id">
    /// Required if inline_message_id is not specified. Identifier of the
    /// sent message
    /// </param>
    /// <param name="inline_message_id">
    /// Required if chat_id and message_id are not specified. Identifier of
    /// the inline message
    /// </param>
    /// <returns>
    /// On success, returns an Array of GameHighScore objects.
    /// </returns>
    /// <remarks>
    /// This method will currently return scores for the target user, plus
    /// two of his closest neighbors on each side. Will also return the top
    /// three users if the user and his neighbors are not among them. Please
    /// note that this behavior is subject to change.
    /// </remarks>
    Function getGameHighScores(user_id: Integer; chat_id: Integer = 0; message_id: Integer = 0;
      Const inline_message_id: string = ''): TArray<TtgGameHighScore>;
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    property IsReceiving: Boolean read fIsReceiving write SetIsReceiving default False;
  published
    { x } property UploadTimeout: Integer read FUploadTimeout write FUploadTimeout default 60000;
    { x } property PollingTimeout: Integer read FPollingTimeout write FPollingTimeout default 1000;
    property MessageOffset: Integer read FMessageOffset write FMessageOffset default 0;
    /// <summary>Монитор слежки за обновлениями</summary>
    property AllowedUpdates: TAllowedUpdates read fAllowedUpdates write fAllowedUpdates
      default UPDATES_ALLOWED_ALL;
    property Token: String read FToken write FToken;
    property OnUpdates: TtgBotOnUpdates read FOnUpdates write FOnUpdates;
    property OnError: TtgBorOnError read FOnError write FOnError;
    property VersionAPI: String read GetVersionAPI;
  End;

implementation

Function ToModeString(Mode: TtgParseMode): String;
Begin
  case Mode of
    TtgParseMode.Default:
      Result := '';
    TtgParseMode.Markdown:
      Result := 'Markdown';
    TtgParseMode.Html:
      Result := 'HTML';
  end;
End;

{$IFDEF TG_NetHttpClient}
{ TTelegramBotNetHttp }

function TTelegramBot.API<T>(const Method: String; Parameters: TDictionary<String, TValue>): T;
var
  lHttp: THTTPClient;
  lHttpResponse: IHTTPResponse;
  lApiResponse: TtgApiResponse<T>;
  lURL_TELEG: String;
  LParamToDate: TMultipartFormData;
begin
  if Self.Token.IsEmpty then
    raise ETelegramTokenEmpty.Create('Token is Empty!');
  lHttp := THTTPClient.Create;
  try
    lURL_TELEG := 'https://api.telegram.org/bot' + FToken + '/' + Method;
    // Преобразовуем параметры в строку, если нужно
    if Assigned(Parameters) then
    Begin
      LParamToDate := ParamsToFormData(Parameters);
      lHttpResponse := lHttp.Post(lURL_TELEG, LParamToDate);
    End
    else
      lHttpResponse := lHttp.Get(lURL_TELEG);
    if lHttpResponse.StatusCode <> 200 then
    begin
      if Assigned(OnError) then
        OnError(Self, lHttpResponse.StatusCode, lHttpResponse.StatusText);
      Exit;
    end;
    lApiResponse := TtgApiResponse<T>.FromJSON(lHttpResponse.ContentAsString);
    if Not lApiResponse.Ok then
    begin
      if Assigned(OnError) then
        OnError(Self, lApiResponse.Code, lApiResponse.Message);
      Exit;
    end;
    Result := lApiResponse.ResultObject;
    lApiResponse.ResultObject := Default (T);
  finally
    if Assigned(Parameters) then
      FreeAndNil(LParamToDate);
    FreeAndNil(lHttp);
    FreeAndNil(lApiResponse);
  end;
end;
{$ELSEIF Defined(TG_INDY)}
{ TMultipartFormDataHelper }

procedure TMultipartFormDataHelper.AddField(const AField, AValue: string);
begin
  AddFormField(AField, AValue);
end;

{ TTelegramBotIndy }

function TTelegramBot.API<T>(const Method: String; Parameters: TDictionary<String, TValue>): T;
var
  lHttp: TIdHTTP;
  lHttpResponse: String;
  lApiResponse: TtgApiResponse<T>;
  lURL_TELEG: String;
  LParamToDate: TMultipartFormData;
begin
  lHttp := TIdHTTP.Create(Self);
  try
    lURL_TELEG := 'https://api.telegram.org/bot' + FToken + '/' + Method;
    // Преобразовуем параметры в строку, если нужно
    if Assigned(Parameters) then
    Begin
      LParamToDate := ParamsToFormData(Parameters);
      lHttpResponse := lHttp.Post(lURL_TELEG, LParamToDate);
    End
    else
      lHttpResponse := lHttp.Get(lURL_TELEG);
    if lHttp.ResponseCode <> 200 then
    begin
      if Assigned(OnError) then
        OnError(Self, lHttp.ResponseCode, lHttp.ResponseText);
      Exit;
    end;
    lApiResponse := TtgApiResponse<T>.FromJSON(lHttpResponse);
    if Not lApiResponse.Ok then
    begin
      if Assigned(OnError) then
        OnError(Self, lApiResponse.Code, lApiResponse.Message);
      Exit;
    end;
    Result := lApiResponse.ResultObject;
    lApiResponse.ResultObject := Default (T);
  finally
    FreeAndNil(LParamToDate);
    FreeAndNil(lHttp);
    FreeAndNil(lApiResponse);
  end;
end;
{$ELSE}
ААААА сложна
{$ENDIF}
{ TTelegram }
function TTelegramBot.AllowedUpdatesToString(allowed_updates: TAllowedUpdates): String;
begin
  Result := '[';
  if TAllowedUpdate.Message in allowed_updates then
    Result := Result + '"message"';
  if TAllowedUpdate.edited_message in allowed_updates then
    Result := Result + '"edited_message"';
  if TAllowedUpdate.channel_post in allowed_updates then
    Result := Result + '"channel_post"';
  if TAllowedUpdate.edited_channel_post in allowed_updates then
    Result := Result + '"edited_channel_post"';
  if TAllowedUpdate.inline_query in allowed_updates then
    Result := Result + '"inline_query"';
  if TAllowedUpdate.chosen_inline_result in allowed_updates then
    Result := Result + '"chosen_inline_result"';
  if TAllowedUpdate.callback_query in allowed_updates then
    Result := Result + '"callback_query"';
  // if TAllowedUpdate.callback_query in allowed_updates then
  // Result := Result + '"edited_message"';
  // if TAllowedUpdate.edited_message in allowed_updates then
  // Result := Result + '"edited_message"';
  Result := Result + ']';
end;

function TTelegramBot.answerCallbackQuery(Const callback_query_id, text: String;
  show_alert: Boolean): Boolean;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('callback_query_id', callback_query_id);
    if NOT text.IsEmpty then
      Parameters.Add('text', text);
    if show_alert then
      Parameters.Add('show_alert', show_alert);
    Result := API<Boolean>('forwardMessage', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.answerInlineQuery(Const inline_query_id: String;
  results: TArray<TtgInlineQueryResult>; cache_time: Integer; is_personal: Boolean;
  Const next_offset, switch_pm_text, switch_pm_parameter: String): Boolean;
var
  Parameters: TDictionary<String, TValue>;
  TestArr: Array of TValue;
  I: Integer;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('inline_query_id', inline_query_id);
    SetLength(TestArr, Length(results));
    for I := Low(results) to High(results) do
      TestArr[I] := results[I];
    Parameters.Add('results', TValue.FromArray(PTypeInfo(TestArr), TestArr));
    Parameters.Add('cache_time', cache_time);
    Parameters.Add('is_personal', is_personal);
    Parameters.Add('next_offset', next_offset);
    Parameters.Add('switch_pm_text', switch_pm_text);
    Parameters.Add('switch_pm_parameter', switch_pm_parameter);
    Result := API<Boolean>('answerInlineQuery', Parameters);
  finally
    Parameters.Free;
  end;
end;

constructor TTelegramBot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecesiver := TtgRecesiver.Create(True);
  AllowedUpdates := UPDATES_ALLOWED_ALL;
  FRecesiver.Bot := Self;
  fIsReceiving := False;
  UploadTimeout := 60000;
  PollingTimeout := 1000;
  MessageOffset := 0;
end;

function TTelegramBot.deleteWebhook: Boolean;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Result := API<Boolean>('deleteWebhook', nil);
  finally
    Parameters.Free;
  end;
end;

destructor TTelegramBot.Destroy;
begin
  Self.PollingTimeout := 0;
  if IsReceiving then
    IsReceiving := False;
  FRecesiver.WaitFor;
  FRecesiver.Free; // Ну почему так долго?!
  inherited;
end;

function TTelegramBot.editMessageCaption(chat_id: TValue; message_id: Integer;
  Const inline_message_id, caption: String; reply_markup: TtgReplyKeyboardMarkup): Boolean;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('message_id', message_id);
    Parameters.Add('inline_message_id', inline_message_id);
    Parameters.Add('caption', caption);
    Parameters.Add('reply_markup', reply_markup);
    Result := API<Boolean>('editMessageCaption', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.editMessageReplyMarkup(chat_id: TValue; message_id: Integer;
  Const inline_message_id: String; reply_markup: TtgReplyKeyboardMarkup): Boolean;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('message_id', message_id);
    Parameters.Add('inline_message_id', inline_message_id);
    Parameters.Add('reply_markup', reply_markup);
    Result := API<Boolean>('editMessageText', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.editMessageText(chat_id: TValue; message_id: Integer;
  Const inline_message_id, text: String; parse_mode: TtgParseMode;
  disable_web_page_preview: Boolean; reply_markup: TtgReplyKeyboardMarkup): Boolean;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('message_id', message_id);
    Parameters.Add('inline_message_id', inline_message_id);
    Parameters.Add('text', text);
    Parameters.Add('parse_mode', ToModeString(parse_mode));
    Parameters.Add('disable_web_page_preview', disable_web_page_preview);
    Parameters.Add('reply_markup', reply_markup);
    Result := API<Boolean>('editMessageText', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.forwardMessage(chat_id, from_chat_id: TValue; disable_notification: Boolean;
  message_id: Integer): TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('from_chat_id', from_chat_id);
    Parameters.Add('disable_notification', disable_notification);
    Parameters.Add('message_id', message_id);
    Result := API<TtgMessage>('forwardMessage', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.getChat(const chat_id: TValue): TtgChat;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Result := Self.API<TtgChat>('getChat', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.getChatAdministrators(const chat_id: TValue): TArray<TtgChatMember>;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Result := Self.API < TArray < TtgChatMember >> ('getChatAdministrators', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.getChatMember(chat_id: TValue; user_id: Integer): TtgChatMember;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('user_id', user_id);
    Result := Self.API<TtgChatMember>('getChatMember', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.getChatMembersCount(const chat_id: TValue): Integer;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Result := Self.API<Integer>('getChatMembersCount', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.getFile(Const file_id: String): TtgFile;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('file_id', file_id);
    Result := Self.API<TtgFile>('getFile', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.getGameHighScores(user_id, chat_id, message_id: Integer;
  Const inline_message_id: string): TArray<TtgGameHighScore>;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('user_id', user_id);
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('message_id', message_id);
    Parameters.Add('inline_message_id', inline_message_id);
    Result := API < TArray < TtgGameHighScore >> ('getGameHighScores', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.getMe: TtgUser;
begin
  Result := Self.API<TtgUser>('getMe', nil);
end;

function TTelegramBot.getUpdates(const offset, limit, timeout: Integer;
  allowed_updates: TAllowedUpdates): TArray<TtgUpdate>;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('offset', offset);
    Parameters.Add('limit', limit);
    Parameters.Add('timeout', timeout);
    Parameters.Add('allowed_updates', AllowedUpdatesToString(allowed_updates));
    Result := API < TArray < TtgUpdate >> ('getUpdates', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.getUserProfilePhotos(chat_id: TValue; offset, limit: Integer)
  : TtgUserProfilePhotos;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('offset', offset);
    Parameters.Add('limit', limit);
    Result := API<TtgUserProfilePhotos>('getUserProfilePhotos', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetVersionAPI: String;
begin
  Result := '2.3.1';
end;

function TTelegramBot.getWebhookInfo: TtgWebhookInfo;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Result := API<TtgWebhookInfo>('getWebhookInfo', nil);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.kickChatMember(chat_id: TValue; user_id: Integer): Boolean;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('user_id', user_id);
    Result := API<Boolean>('kickChatMember', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.leaveChat(chat_id: TValue): Boolean;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Result := API<Boolean>('leaveChat', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.ParamsToFormData(Parameters: TDictionary<String, TValue>): TMultipartFormData;
var
  parameter: TPair<String, TValue>;
begin
  Result := TMultipartFormData.Create;
  for parameter in Parameters do
  begin
    if parameter.Value.IsType<TtgInlineKeyboardMarkup> then
    begin
      { TODO -oOwner -cGeneral : Проверить че за херня тут твориться }
      if parameter.Value.AsType<TtgInlineKeyboardMarkup> <> nil then
        Result.AddField(parameter.Key, parameter.Value.AsType<TtgInlineKeyboardMarkup>.AsJSON);
    end
    else if parameter.Value.IsType<TtgReplyKeyboardMarkup> then
    begin
      if parameter.Value.AsType<TtgReplyKeyboardMarkup> <> nil then
        Result.AddField(parameter.Key, parameter.Value.AsType<TtgReplyKeyboardMarkup>.AsJSON);
    end
    else if parameter.Value.IsType<TtgReplyKeyboardHide> then
    begin
      if parameter.Value.AsType<TtgReplyKeyboardHide> <> nil then
        Result.AddField(parameter.Key, parameter.Value.AsType<TtgReplyKeyboardHide>.AsJSON);
    end
    else if parameter.Value.IsType<TtgForceReply> then
    begin
      if parameter.Value.AsType<TtgForceReply> <> nil then
        Result.AddField(parameter.Key, parameter.Value.AsType<TtgForceReply>.AsJSON);
    end
    else if parameter.Value.IsType<TtgFileToSend> then
    Begin
      { TODO -oOwner -cGeneral : Отправка файлов }
      Result.AddFile(parameter.Key, parameter.Value.AsType<TtgFileToSend>.FileName);
    End
    else if parameter.Value.IsType<string> then
    Begin
      if NOT parameter.Value.AsString.IsEmpty then
        Result.AddField(parameter.Key, parameter.Value.AsString)
    End
    else if parameter.Value.IsType<Int64> then
    Begin
      if parameter.Value.AsInt64 <> 0 then
        Result.AddField(parameter.Key, IntToStr(parameter.Value.AsInt64));
    End
    else if parameter.Value.IsType<Boolean> then
    Begin
      if parameter.Value.AsBoolean then
        Result.AddField(parameter.Key, TtgUtils.IfThen<String>(parameter.Value.AsBoolean,
          'true', 'false'))
    End
    else
      raise ETelegramUnknownData.Create('Check parametr type ' + parameter.Value.ToString);
  end;
end;

function TTelegramBot.sendAudio(chat_id, audio: TValue; duration: Integer;
  Const performer, title: String; disable_notification: Boolean; reply_to_message_id: Integer;
  replyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('audio', audio);
    Parameters.Add('duration', duration);
    Parameters.Add('performer', performer);
    Parameters.Add('title', title);
    Parameters.Add('disable_notification', disable_notification);
    Parameters.Add('reply_to_message_id', reply_to_message_id);
    Parameters.Add('reply_markup', replyMarkup);
    Result := API<TtgMessage>('sendAudio', Parameters);
  finally
    Parameters.Free;
  end;
end;

procedure TTelegramBot.sendChatAction(chat_id: TValue; Const action: String);
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('action', action);
    API<Boolean>('sendChatAction', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.sendContact(chat_id: TValue; contact: TtgContact;
  disable_notification: Boolean; reply_to_message_id: Integer; reply_markup: TtgReplyKeyboardMarkup)
  : TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('phone_number', contact.PhoneNumber);
    Parameters.Add('first_name', contact.FirstName);
    Parameters.Add('last_name', contact.LastName);
    Parameters.Add('disable_notification', disable_notification);
    Parameters.Add('reply_to_message_id', reply_to_message_id);
    Parameters.Add('reply_markup', reply_markup);
    Result := API<TtgMessage>('sendContact', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.sendDocument(chat_id, document: TValue; Const caption: String;
  disable_notification: Boolean; reply_to_message_id: Integer; reply_markup: TtgReplyKeyboardMarkup)
  : TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('document', document);
    Parameters.Add('caption', caption);
    Parameters.Add('disable_notification', disable_notification);
    Parameters.Add('reply_to_message_id', reply_to_message_id);
    Parameters.Add('reply_markup', reply_markup);
    Result := API<TtgMessage>('sendDocument', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.sendGame(chat_id: Integer; Const game_short_name: String;
  disable_notification: Boolean; reply_to_message_id: Integer; reply_markup: TtgReplyKeyboardMarkup)
  : TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('game_short_name', game_short_name);
    Parameters.Add('disable_notification', disable_notification);
    Parameters.Add('reply_to_message_id', reply_to_message_id);
    Parameters.Add('reply_markup', reply_markup);
    Result := API<TtgMessage>('sendGame', Parameters);
  finally
    Parameters.Free;
  end;

end;

function TTelegramBot.sendLocation(chat_id: TValue; Location: TtgLocation;
  disable_notification: Boolean; reply_to_message_id: Integer; reply_markup: TtgReplyKeyboardMarkup)
  : TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('latitude', Location.Latitude);
    Parameters.Add('longitude', Location.Longitude);
    Parameters.Add('disable_notification', disable_notification);
    Parameters.Add('reply_to_message_id', reply_to_message_id);
    Parameters.Add('reply_markup', reply_markup);
    Result := API<TtgMessage>('sendLocation', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.sendPhoto(chatId, photo: TValue; Const caption: string;
  disable_notification: Boolean; replyToMessageId: Integer; replyMarkup: TtgReplyKeyboardMarkup)
  : TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chatId);
    Parameters.Add('photo', photo);
    Parameters.Add('caption', caption);
    Parameters.Add('disable_notification', disable_notification);
    Parameters.Add('reply_to_message_id', replyToMessageId);
    Parameters.Add('reply_markup', replyMarkup);
    Result := API<TtgMessage>('sendPhoto', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.sendSticker(chat_id, sticker: TValue; Const caption: String;
  disable_notification: Boolean; reply_to_message_id: Integer; reply_markup: TtgReplyKeyboardMarkup)
  : TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('sticker', sticker);
    Parameters.Add('caption', caption);
    Parameters.Add('disable_notification', disable_notification);
    Parameters.Add('reply_to_message_id', reply_to_message_id);
    Parameters.Add('reply_markup', reply_markup);
    Result := API<TtgMessage>('sendSticker', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.sendMessage(const chat_id: TValue; const text: String;
  ParseMode: TtgParseMode; disableWebPagePreview, disable_notification: Boolean;
  replyToMessageId: Integer; replyMarkup: TtgReplyMarkup): TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('text', text);
    Parameters.Add('parse_mode', ToModeString(ParseMode));
    Parameters.Add('disable_web_page_preview', disableWebPagePreview);
    Parameters.Add('disable_notification', disable_notification);
    Parameters.Add('reply_to_message_id', replyToMessageId);
    Parameters.Add('reply_markup', replyMarkup);
    Result := API<TtgMessage>('sendMessage', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.sendVenue(chat_id: TValue; venue: TtgVenue; disable_notification: Boolean;
  reply_to_message_id: Integer; reply_markup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('latitude', venue.Location.Latitude);
    Parameters.Add('longitude', venue.Location.Longitude);
    Parameters.Add('title', venue.title);
    Parameters.Add('address', venue.Address);
    Parameters.Add('foursquare_id', venue.FoursquareId);
    Parameters.Add('disable_notification', disable_notification);
    Parameters.Add('reply_to_message_id', reply_to_message_id);
    Parameters.Add('reply_markup', reply_markup);
    Result := API<TtgMessage>('sendVenue', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.sendVideo(chat_id, video: TValue; duration, width, height: Integer;
  const caption: String; disable_notification: Boolean; reply_to_message_id: Integer;
  reply_markup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('video', video);
    Parameters.Add('duration', duration);
    Parameters.Add('width', width);
    Parameters.Add('height', height);
    Parameters.Add('caption', caption);
    Parameters.Add('disable_notification', disable_notification);
    Parameters.Add('reply_to_message_id', reply_to_message_id);
    Parameters.Add('reply_markup', reply_markup);
    Result := API<TtgMessage>('sendVideo', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.sendVoice(chat_id, voice: TValue; duration: Integer;
  disable_notification: Boolean; reply_to_message_id: Integer; reply_markup: TtgReplyKeyboardMarkup)
  : TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('voice', voice);
    Parameters.Add('duration', duration);
    Parameters.Add('disable_notification', disable_notification);
    Parameters.Add('reply_to_message_id', reply_to_message_id);
    Parameters.Add('reply_markup', reply_markup);
    Result := API<TtgMessage>('sendVoice', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.setGameScore(user_id, score: Integer; force, disable_edit_message: Boolean;
  chat_id, message_id: Integer; Const inline_message_id: String): TtgMessage;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('user_id', user_id);
    Parameters.Add('score', score);
    Parameters.Add('force', force);
    Parameters.Add('disable_edit_message', disable_edit_message);
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('message_id', message_id);
    Parameters.Add('inline_message_id', inline_message_id);
    Result := API<TtgMessage>('setGameScore', Parameters);
  finally
    Parameters.Free;
  end;
end;

procedure TTelegramBot.SetIsReceiving(const Value: Boolean);
begin
  if (csDesigning in ComponentState) then
    Exit;
  fIsReceiving := Value;
  if Value then
    FRecesiver.Start
  else
    FRecesiver.Terminate;
end;

procedure TTelegramBot.setWebhook(const url: String; certificate: TtgFileToSend;
  max_connections: Integer; allowed_updates: TAllowedUpdates);
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('url', url);
    Parameters.Add('certificate', certificate);
    Parameters.Add('max_connections', max_connections);
    Parameters.Add('allowed_updates', AllowedUpdatesToString(allowed_updates));
    API<Boolean>('setWebhook', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.unbanChatMember(chat_id: TValue; user_id: Integer): Boolean;
var
  Parameters: TDictionary<String, TValue>;
begin
  Parameters := TDictionary<String, TValue>.Create;
  try
    Parameters.Add('chat_id', chat_id);
    Parameters.Add('user_id', user_id);
    Result := API<Boolean>('unbanChatMember', Parameters);
  finally
    Parameters.Free;
  end;
end;

procedure TtgRecesiver.Execute;
var
  LUpdates: TArray<TtgUpdate>;
Begin
  repeat
    Sleep(fBot.PollingTimeout);
    if (Terminated) or (NOT fBot.IsReceiving) then
      Break;
    LUpdates := fBot.getUpdates(Bot.MessageOffset);
    if Length(LUpdates) = 0 then
      Continue;
    Bot.MessageOffset := LUpdates[High(LUpdates)].Id + 1;
    TThread.Queue(Self,
      procedure
      var
        I: Integer;
      begin
        if Assigned(Bot.OnUpdates) and Assigned(LUpdates) then
          fBot.OnUpdates(Self, LUpdates);
        if Assigned(LUpdates) then
        Begin
          for I := Low(LUpdates) to High(LUpdates) do
            FreeAndNil(LUpdates[I]) { .Free };
          LUpdates := nil;
        end;
      end);
  until (Terminated) or (NOT Bot.IsReceiving);
end;

end.
