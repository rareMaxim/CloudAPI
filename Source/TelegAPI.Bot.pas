unit TelegAPI.Bot;

interface

uses
  System.Generics.Collections,
  System.Rtti,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Net.Mime,
  System.Net.HttpClient,
  TelegAPI.Classes,
  XSuperObject;

type
  TTelegramBot = class;

  TtgRecesiver = class( TThread )
    private
      FBot : TTelegramBot;
    protected
      procedure Execute; override;
    public
      property Bot : TTelegramBot
        read FBot
        write FBot;
  end;

  /// <summary>
  /// Для приема обновл
  /// </summary>
  TtgBotOnUpdates = procedure(
    Sender  : TTelegramBot;
    Updates : TArray< TtgUpdate > ) of object;
  /// <summary>
  /// Вызывается при возникновении ошибки
  /// </summary>
  TtgBorOnError = procedure(
    Sender        : TTelegramBot;
    const Code    : Integer;
    const Message : string ) of object;

  TTelegramBotCore = class( TComponent )
    private
      FToken : string;
      FOnUpdates : TtgBotOnUpdates;
      FPollingTimeout : Integer;
      FMessageOffset : Integer;
      FOnError : TtgBorOnError;
      FRecesiver : TtgRecesiver;
      FIsReceiving : Boolean;
      FAllowedUpdates : TAllowedUpdates;
      procedure SetIsReceiving( const Value : Boolean );
      function GetVersionAPI : string;
      /// <summary>
      /// Мастер-функция для запросов на сервак
      /// </summary>
      function API< T >(
        const Method : string;
        Parameters   : TDictionary< string, TValue > ) : T;
      function ParamsToFormData( Parameters : TDictionary< string, TValue > )
        : TMultipartFormData;
      function ArrayToString< T : class, constructor >
        ( LArray : TArray< T > ) : string;
    public
      constructor Create( AOwner : TComponent ); overload; override;
      destructor Destroy; override;
      /// <summary>
      /// Ассинхронный прием обновлений от сервера
      /// </summary>
      property IsReceiving : Boolean
        read FIsReceiving
        write SetIsReceiving
        default False;
    published
      /// <summary>
      /// Задержка между опросами
      /// </summary>
      property PollingTimeout : Integer
        read FPollingTimeout
        write FPollingTimeout
        default 1000;
      property MessageOffset : Integer
        read FMessageOffset
        write FMessageOffset
        default 0;
      /// <summary>
      /// Типы принимаемых сообщений
      /// </summary>
      /// <example>
      /// 283107813:AAG4hEElAvIogTSHNHXI6rZtE46A7XQvIH
      /// </example>
      property AllowedUpdates : TAllowedUpdates
        read FAllowedUpdates
        write FAllowedUpdates
        default UPDATES_ALLOWED_ALL;
      /// <summary>
      /// Токен вашего бота.
      /// </summary>
      /// <remarks>
      /// Создать бота и получить токен можно у @BotFather
      /// </remarks>
      property Token : string
        read FToken
        write FToken;
      /// <summary>
      /// Для получения обновлений. Тип обновлений можно указать в <see cref="TelegAPI.Bot|TTelegramBotCore.AllowedUpdates">
      /// AllowedUpdates</see>
      /// </summary>
      property OnUpdates : TtgBotOnUpdates
        read FOnUpdates
        write FOnUpdates;
      /// <summary>
      /// Получение сведений об ошибках, возникших в процессе работы
      /// </summary>
      property OnError : TtgBorOnError
        read FOnError
        write FOnError;
      /// <summary>
      /// Поддерживаемая версия платформы BotAPI
      /// </summary>
      property VersionAPI : string
        read GetVersionAPI;
  end;

  TTelegramBot = class( TTelegramBotCore )
    public
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
      /// Identifier of the first update to be returned. Must be greater by
      /// one than the highest among the identifiers of previously received
      /// updates. By default, updates starting with the earliest unconfirmed
      /// update are returned. An update is considered confirmed as soon as <see cref="TelegAPI.Bot|TTelegramBot.GetUpdates(Integer,Integer,Integer,TAllowedUpdates)">
      /// getUpdates</see> is called with an offset higher than its
      /// update_id. The negative offset can be specified to retrieve updates
      /// starting from -offset update from the end of the updates queue. All
      /// previous updates will forgotten.
      /// </param>
      /// <param name="Limit">
      /// Количество обновлений которые могут прийти в одном запросе.
      /// Допустимое значение от 1 до 100. По умолчанию - 100.Limits the
      /// number of updates to be retrieved. Values between 1—100 are
      /// accepted. Defaults to 100.
      /// </param>
      /// <param name="Timeout">
      /// Timeout in seconds for long polling. Defaults to 0, i.e. usual
      /// short polling
      /// </param>
      /// <param name="Allowed_updates">
      /// List the types of updates you want your bot to receive. For
      /// example, specify [“message”, “edited_channel_post”,
      /// “callback_query”] to only receive updates of these types. See <see cref="TelegAPi.Classes|TtgUpdate">
      /// Update</see> for a complete list of available update types. Specify
      /// an empty list to receive all updates regardless of type (default).
      /// If not specified, the previous setting will be used. <br /><br />
      /// Please note that this parameter doesn't affect updates created
      /// before the call to the getUpdates, so unwanted updates may be
      /// received for a short period of time.
      /// </param>
      /// <returns>
      /// An Array of Update objects is returned.
      /// </returns>
      /// <remarks>
      /// 1. This method will not work if an outgoing webhook is set up. 2.
      /// In order to avoid getting duplicate updates, recalculate offset
      /// after each server response.
      /// </remarks>
      function GetUpdates(
        const Offset    : Integer = 0;
        const Limit     : Integer = 100;
        const Timeout   : Integer = 0;
        Allowed_updates : TAllowedUpdates = UPDATES_ALLOWED_ALL )
        : TArray< TtgUpdate >;
      /// <summary>
      /// Use this method to specify a url and receive incoming updates via
      /// an outgoing webhook. Whenever there is an update for the bot, we
      /// will send an HTTPS POST request to the specified url, containing a
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
      /// <param name="Max_connections">
      /// Maximum allowed number of simultaneous HTTPS connections to the
      /// webhook for update delivery, 1-100. Defaults to 40. Use lower
      /// values to limit the load on your bot‘s server, and higher values to
      /// increase your bot’s throughput.
      /// </param>
      /// <param name="Allowed_updates">
      /// List the types of updates you want your bot to receive. For
      /// example, specify [“message”, “edited_channel_post”,
      /// “callback_query”] to only receive updates of these types. See <see cref="TelegAPi.Classes|TtgUpdate">
      /// Update</see> for a complete list of available update types. Specify
      /// an empty list to receive all updates regardless of type (default).
      /// If not specified, the previous setting will be used. <br /><br />
      /// Please note that this parameter doesn't affect updates created
      /// before the call to the setWebhook, so unwanted updates may be
      /// received for a short period of time.
      /// </param>
      /// <remarks>
      /// <para>
      /// Notes
      /// </para>
      /// <para>
      /// 1. You will not be able to receive updates using <see cref="TelegAPI.Bot|TTelegramBot.GetUpdates(Integer,Integer,Integer,TAllowedUpdates)">
      /// getUpdates</see> for as long as an outgoing webhook is set up.
      /// </para>
      /// <para>
      /// 2. To use a self-signed certificate, you need to upload your <see href="https://core.telegram.org/bots/self-signed">
      /// public key certificate</see> using <c>certificate</c>
      /// parameter. Please upload as InputFile, sending a String will
      /// not work.
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
      procedure SetWebhook(
        const Url       : string;
        Certificate     : TtgFileToSend = nil;
        Max_connections : Integer = 40;
        Allowed_updates : TAllowedUpdates = UPDATES_ALLOWED_ALL );
      /// <summary>
      /// Use this method to remove webhook integration if you decide to
      /// switch back to <see cref="TelegAPI.Bot|TTelegramBot.GetUpdates(Integer,Integer,Integer,TAllowedUpdates)">
      /// getUpdates</see>.
      /// </summary>
      /// <returns>
      /// Returns <c>True</c> on success.
      /// </returns>
      /// <seealso cref="TelegAPI.Bot|TTelegramBot.setWebhook(string,TtgFileToSend,TAllowedUpdates)">
      /// getUpdates
      /// </seealso>
      function DeleteWebhook : Boolean;

      /// <summary>
      /// Use this method to get current webhook status.
      /// </summary>
      /// <returns>
      /// On success, returns a <see cref="TelegAPi.Classes|TtgWebhookInfo">
      /// WebhookInfo</see> object
      /// </returns>
      /// <remarks>
      /// If the bot is using <see cref="TelegAPI.Bot|TTelegramBot.GetUpdates(Integer,Integer,Integer,TAllowedUpdates)">
      /// getUpdates</see>, will return an object with the url field empty
      /// </remarks>
      function GetWebhookInfo : TtgWebhookInfo;
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
      /// Returns basic information about the bot in form of a <see cref="TelegAPi.Classes|TtgUser">
      /// User</see> object.
      /// </para>
      /// </returns>
      function GetMe : TtgUser;
      /// <summary>
      /// Use this method to send text messages.
      /// </summary>
      /// <param name="Chat_id">
      /// Integer or String. Unique identifier for the target chat or
      /// username of the target channel (in the format <c>@channelusername</c>
      /// ).
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
      /// <param name="Disable_notification">
      /// Sends the message <see href="https://telegram.org/blog/channels-2-0#silent-messages">
      /// silently</see>. iOS users will not receive a notification, Android
      /// users will receive a notification with no sound.
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
      /// On success, the sent Message is returned.
      /// </returns>
      function SendMessage(
        const Chat_id         : TValue;
        const Text            : string;
        ParseMode             : TtgParseMode = TtgParseMode.Default;
        DisableWebPagePreview : Boolean = False;
        Disable_notification  : Boolean = False;
        ReplyToMessageId      : Integer = 0;
        ReplyMarkup           : TtgReplyMarkup = nil ) : TtgMessage;

      /// <summary>
      /// Use this method to forward messages of any kind.
      /// </summary>
      /// <param name="Chat_id">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="From_chat_id">
      /// Unique identifier for the chat where the original message was sent
      /// (or channel username in the format @channelusername)
      /// </param>
      /// <param name="Disable_notification">
      /// Sends the message silently. iOS users will not receive a
      /// notification, Android users will receive a notification with no
      /// sound.
      /// </param>
      /// <param name="Message_id">
      /// Unique message identifier
      /// </param>
      /// <returns>
      /// On success, the sent Message is returned.
      /// </returns>
      function ForwardMessage(
        Chat_id              : TValue;
        From_chat_id         : TValue;
        Disable_notification : Boolean = False;
        Message_id           : Integer = 0 ) : TtgMessage;
      /// <summary>
      /// Use this method to send photos.
      /// </summary>
      /// <param name="chatId">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="photo">
      /// Photo to send. You can either pass a file_id as String to resend a
      /// photo that is already on the Telegram servers, or upload a new
      /// photo using multipart/form-data.
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
      /// Additional interface options. A JSON-serialized object for an
      /// inline keyboard, custom reply keyboard, instructions to remove
      /// reply keyboard or to force a reply from the user.
      /// </param>
      /// <returns>
      /// On success, the sent Message is returned.
      /// </returns>
      /// <example>
      /// var <br />LMessage: TtgMessage; <br />Begin <br />//Если не
      /// известен ИД файла <br />LMessage := sendPhoto(chatId,
      /// TtgFileToSend.Create('Путь к файлу'), nil); <br />//Если известен
      /// ИД файла <br />LMessage := sendPhoto(chatId, 'ИД Файла'); <br />...
      /// <br /> LMessage.Free; <br />End;
      /// </example>
      function SendPhoto(
        ChatId               : TValue;
        Photo                : TValue;
        const Caption        : string = '';
        Disable_notification : Boolean = False;
        ReplyToMessageId     : Integer = 0;
        ReplyMarkup          : TtgReplyKeyboardMarkup = nil ) : TtgMessage;
      /// <summary>
      /// Use this method to send audio files, if you want Telegram clients
      /// to display them in the music player. Your audio must be in the .mp3
      /// format.
      /// </summary>
      /// <returns>
      /// On success, the sent Message is returned.
      /// </returns>
      /// <remarks>
      /// Bots can currently send audio files of up to 50 MB in size, this
      /// limit may be changed in the future. For sending voice messages, use
      /// the sendVoice method instead.
      /// </remarks>
      /// <param name="chat_id">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="audio">
      /// Audio file to send. You can either pass a file_id as String to
      /// resend an audio that is already on the Telegram servers, or upload
      /// a new audio file using multipart/form-data.
      /// </param>
      /// <param name="duration">
      /// Duration of the audio in seconds
      /// </param>
      /// <param name="performer">
      /// Performer
      /// </param>
      /// <param name="title">
      /// Track name
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
      /// Additional interface options. A JSON-serialized object for an
      /// inline keyboard, custom reply keyboard, instructions to hide reply
      /// keyboard or to force a reply from the user.
      /// </param>
      function SendAudio(
        Chat_id              : TValue;
        Audio                : TValue;
        Duration             : Integer = 0;
        const Performer      : string = '';
        const Title          : string = '';
        Disable_notification : Boolean = False;
        Reply_to_message_id  : Integer = 0;
        ReplyMarkup          : TtgReplyKeyboardMarkup = nil ) : TtgMessage;
      /// <summary>
      /// Use this method to send general files.
      /// </summary>
      /// <returns>
      /// On success, the sent Message is returned.
      /// </returns>
      /// <remarks>
      /// Bots can currently send files of any type of up to 50 MB in size,
      /// this limit may be changed in the future.
      /// </remarks>
      /// <param name="chat_id">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="document">
      /// File to send. You can either pass a file_id as String to resend a
      /// file that is already on the Telegram servers, or upload a new file
      /// using multipart/form-data.
      /// </param>
      /// <param name="caption">
      /// Document caption (may also be used when resending documents by
      /// file_id), 0-200 characters
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
      /// Additional interface options. A JSON-serialized object for an
      /// inline keyboard, custom reply keyboard, instructions to hide reply
      /// keyboard or to force a reply from the user.
      /// </param>
      function SendDocument(
        Chat_id              : TValue;
        Document             : TValue;
        const Caption        : string = '';
        Disable_notification : Boolean = False;
        Reply_to_message_id  : Integer = 0;
        Reply_markup         : TtgReplyKeyboardMarkup = nil ) : TtgMessage;
      /// <summary>
      /// Use this method to send .webp stickers.
      /// </summary>
      /// <returns>
      /// On success, the sent Message is returned.
      /// </returns>
      /// <remarks />
      /// <param name="chat_id">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="sticker">
      /// Sticker to send. You can either pass a file_id as String to resend
      /// a sticker that is already on the Telegram servers, or upload a new
      /// sticker using multipart/form-data.
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
      /// Additional interface options. A JSON-serialized object for an
      /// inline keyboard, custom reply keyboard, instructions to hide reply
      /// keyboard or to force a reply from the user.
      /// </param>
      function SendSticker(
        Chat_id              : TValue;
        Sticker              : TValue;
        const Caption        : string = '';
        Disable_notification : Boolean = False;
        Reply_to_message_id  : Integer = 0;
        Reply_markup         : TtgReplyKeyboardMarkup = nil ) : TtgMessage;
      /// <summary>
      /// Use this method to send video files, Telegram clients support mp4
      /// videos (other formats may be sent as Document).
      /// </summary>
      /// <returns>
      /// On success, the sent Message is returned.
      /// </returns>
      /// <remarks>
      /// Bots can currently send video files of up to 50 MB in size, this
      /// limit may be changed in the future.
      /// </remarks>
      /// <param name="chat_id">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="video">
      /// Video to send. You can either pass a file_id as String to resend a
      /// video that is already on the Telegram servers, or upload a new
      /// video file using multipart/form-data.
      /// </param>
      /// <param name="duration">
      /// Duration of sent video in seconds
      /// </param>
      /// <param name="width">
      /// Video width
      /// </param>
      /// <param name="height">
      /// Video height
      /// </param>
      /// <param name="caption">
      /// Video caption (may also be used when resending videos by file_id),
      /// 0-200 characters
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
      /// Additional interface options. A JSON-serialized object for an
      /// inline keyboard, custom reply keyboard, instructions to hide reply
      /// keyboard or to force a reply from the user.
      /// </param>
      function SendVideo(
        Chat_id              : TValue;
        Video                : TValue;
        Duration             : Integer = 0;
        Width                : Integer = 0;
        Height               : Integer = 0;
        const Caption        : string = '';
        Disable_notification : Boolean = False;
        Reply_to_message_id  : Integer = 0;
        Reply_markup         : TtgReplyKeyboardMarkup = nil ) : TtgMessage;

      /// <summary>
      /// Use this method to send audio files, if you want Telegram clients
      /// to display the file as a playable voice message. For this to work,
      /// your audio must be in an .ogg file encoded with OPUS (other formats
      /// may be sent as Audio or Document).
      /// </summary>
      /// <returns>
      /// On success, the sent Message is returned
      /// </returns>
      /// <remarks>
      /// Bots can currently send voice messages of up to 50 MB in size, this
      /// limit may be changed in the future.
      /// </remarks>
      /// <param name="chat_id">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="voice">
      /// Audio file to send. You can either pass a file_id as String to
      /// resend an audio that is already on the Telegram servers, or upload
      /// a new audio file using multipart/form-data.
      /// </param>
      /// <param name="duration">
      /// Duration of sent audio in seconds
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
      /// Additional interface options. A JSON-serialized object for an
      /// inline keyboard, custom reply keyboard, instructions to hide reply
      /// keyboard or to force a reply from the user.
      /// </param>
      function SendVoice(
        Chat_id              : TValue;
        Voice                : TValue;
        Duration             : Integer = 0;
        Disable_notification : Boolean = False;
        Reply_to_message_id  : Integer = 0;
        Reply_markup         : TtgReplyKeyboardMarkup = nil ) : TtgMessage;
      /// <summary>
      /// As of <see href="https://telegram.org/blog/video-messages-and-telescope">
      /// v.4.0</see>, Telegram clients support rounded square mp4 videos of
      /// up to 1 minute long.
      /// </summary>
      /// <param name="chat_id">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="video_note">
      /// Video note to send. Pass a file_id as String to send a video note
      /// that exists on the Telegram servers (recommended) or upload a new
      /// video using multipart/form-data. More info on Sending Files ».
      /// Sending video notes by a URL is currently unsupported
      /// </param>
      /// <param name="duration">
      /// Duration of sent video in seconds
      /// </param>
      /// <param name="length">
      /// Video width and height
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
      /// Additional interface options. A JSON-serialized object for an
      /// inline keyboard, custom reply keyboard, instructions to remove
      /// reply keyboard or to force a reply from the user.
      /// </param>
      /// <returns>
      /// On success, the sent Message is returned.
      /// </returns>
      /// <remarks>
      /// Use this method to send video messages.
      /// </remarks>
      function SendVideoNote(
        Chat_id              : TValue; //
        Video_note           : TValue; //
        Duration             : Integer = 0; //
        Length               : Integer = 0; //
        Disable_notification : Boolean = False; //
        Reply_to_message_id  : Integer = 0; //
        Reply_markup         : TtgReplyKeyboardMarkup = nil //
        ) : TtgMessage;

      /// <summary>
      /// Use this method to send point on the map.
      /// </summary>
      /// <returns>
      /// On success, the sent Message is returned.
      /// </returns>
      /// <remarks />
      /// <param name="chat_id">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="latitude">
      /// Latitude of location
      /// </param>
      /// <param name="longitude">
      /// Longitude of location
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
      /// Additional interface options. A JSON-serialized object for an
      /// inline keyboard, custom reply keyboard, instructions to hide reply
      /// keyboard or to force a reply from the user.
      /// </param>
      /// <param name="" />
      function SendLocation(
        Chat_id              : TValue;
        Location             : TtgLocation;
        Disable_notification : Boolean = False;
        Reply_to_message_id  : Integer = 0;
        Reply_markup         : TtgReplyKeyboardMarkup = nil ) : TtgMessage;
      /// <summary>
      /// Use this method to send information about a venue.
      /// </summary>
      /// <returns>
      /// On success, the sent Message is returned.
      /// </returns>
      /// <remarks />
      /// <param name="chat_id">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="latitude">
      /// Latitude of the venue
      /// </param>
      /// <param name="longitude">
      /// Longitude of the venue
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
      /// <param name="disable_notification">
      /// Sends the message silently. iOS users will not receive a
      /// notification, Android users will receive a notification with no
      /// sound.
      /// </param>
      /// <param name="reply_to_message_id">
      /// If the message is a reply, ID of the original message
      /// </param>
      /// <param name="reply_markup">
      /// Additional interface options. A JSON-serialized object for an
      /// inline keyboard, custom reply keyboard, instructions to hide reply
      /// keyboard or to force a reply from the user.
      /// </param>
      function SendVenue(
        Chat_id              : TValue;
        Venue                : TtgVenue;
        Disable_notification : Boolean = False;
        Reply_to_message_id  : Integer = 0;
        Reply_markup         : TtgReplyKeyboardMarkup = nil ) : TtgMessage;
      /// <summary>
      /// Use this method to send phone contacts.
      /// </summary>
      /// <returns>
      /// On success, the sent Message is returned.
      /// </returns>
      /// <param name="chat_id">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="phone_number">
      /// Contact's phone number
      /// </param>
      /// <param name="first_name">
      /// Contact's first name
      /// </param>
      /// <param name="last_name">
      /// Contact's last name
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
      /// Additional interface options. A JSON-serialized object for an
      /// inline keyboard, custom reply keyboard, instructions to hide
      /// keyboard or to force a reply from the user.
      /// </param>
      function SendContact(
        Chat_id              : TValue;
        Contact              : TtgContact;
        Disable_notification : Boolean = False;
        Reply_to_message_id  : Integer = 0;
        Reply_markup         : TtgReplyKeyboardMarkup = nil ) : TtgMessage;
      /// <summary>
      /// Use this method when you need to tell the user that something is
      /// happening on the bot's side. The status is set for 5 seconds or
      /// less (when a message arrives from your bot, Telegram clients clear
      /// its typing status).
      /// </summary>
      /// <remarks>
      /// We only recommend using this method when a response from the bot
      /// will take a noticeable amount of time to arrive.
      /// </remarks>
      /// <param name="chat_id">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="action">
      /// Type of action to broadcast. Choose one, depending on what the user
      /// is about to receive: typing for text messages, upload_photo for
      /// photos, record_video or upload_video for videos, record_audio or
      /// upload_audio for audio files, upload_document for general files,
      /// find_location for location data
      /// </param>
      procedure SendChatAction(
        Chat_id      : TValue;
        const Action : string );
      /// <summary>
      /// Use this method to get a list of profile pictures for a user.
      /// </summary>
      /// <param name="chat_id">
      /// Unique identifier of the target user <br />
      /// </param>
      /// <param name="offset">
      /// Sequential number of the first photo to be returned. By default,
      /// all photos are returned.
      /// </param>
      /// <param name="limit">
      /// Limits the number of photos to be retrieved. Values between 1—100
      /// are accepted. Defaults to 100.
      /// </param>
      /// <returns>
      /// Returns a UserProfilePhotos object.
      /// </returns>
      function GetUserProfilePhotos(
        Chat_id : TValue;
        Offset  : Integer;
        Limit   : Integer = 100 ) : TtgUserProfilePhotos;
      /// <summary>
      /// Use this method to get basic info about a file and prepare it for
      /// downloading. For the moment, bots can download files of up to 20MB
      /// in size.
      /// </summary>
      /// <returns>
      /// On success, a File object is returned.
      /// </returns>
      /// <param name="file_id">
      /// File identifier to get info about
      /// </param>
      function GetFile( const File_id : string ) : TtgFile;
      /// <summary>
      /// Use this method to kick a user from a group or a supergroup. In the
      /// case of supergroups, the user will not be able to return to the
      /// group on their own using invite links, etc., unless unbanned first.
      /// The bot must be an administrator in the group for this to work.
      /// </summary>
      /// <returns>
      /// Returns True on success.
      /// </returns>
      /// <remarks>
      /// Note: This will method only work if the ‘All Members Are Admins’
      /// setting is off in the target group. Otherwise members may only be
      /// removed by the group's creator or by the member that added them.
      /// </remarks>
      /// <param name="chat_id">
      /// Unique identifier for the target group or username of the target
      /// supergroup (in the format @supergroupusername)
      /// </param>
      /// <param name="user_id">
      /// Unique identifier of the target user
      /// </param>
      function KickChatMember(
        Chat_id : TValue;
        User_id : Integer ) : Boolean;
      /// <summary>
      /// Use this method to unban a previously kicked user in a supergroup.
      /// The user will not return to the group automatically, but will be
      /// able to join via link, etc. The bot must be an administrator in the
      /// group for this to work.
      /// </summary>
      /// <returns>
      /// Returns True on success.
      /// </returns>
      /// <remarks />
      /// <param name="chat_id">
      /// Unique identifier for the target group or username of the target
      /// supergroup (in the format @supergroupusername)
      /// </param>
      /// <param name="user_id">
      /// Unique identifier of the target user
      /// </param>
      function UnbanChatMember(
        Chat_id : TValue;
        User_id : Integer ) : Boolean;

      /// <summary>
      /// Use this method for your bot to leave a group, supergroup or
      /// channel.
      /// </summary>
      /// <param name="chat_id">
      /// Unique identifier for the target group or username of the target
      /// supergroup (in the format @supergroupusername)
      /// </param>
      /// <returns>
      /// Returns True on success.
      /// </returns>
      function LeaveChat( Chat_id : TValue ) : Boolean;

      /// <summary>
      /// Use this method to get up to date information about the chat
      /// (current name of the user for one-on-one conversations, current
      /// username of a user, group or channel, etc.)
      /// </summary>
      /// <param name="chat_id">
      /// Unique identifier for the target chat or username of the target
      /// supergroup or channel (in the format @channelusername)
      /// </param>
      /// <returns>
      /// Returns a Chat object on success.
      /// </returns>
      function GetChat( const Chat_id : TValue ) : TtgChat;
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
      /// chat is a group or a supergroup and no administrators were
      /// appointed, only the creator will be returned.
      /// </returns>
      function GetChatAdministrators( const Chat_id : TValue )
        : TArray< TtgChatMember >;
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
      function GetChatMembersCount( const Chat_id : TValue ) : Integer;
      /// <summary>
      /// Use this method to get information about a member of a chat.
      /// </summary>
      /// <returns>
      /// Returns a ChatMember object on success.
      /// </returns>
      /// <param name="chat_id">
      /// Unique identifier for the target group or username of the target
      /// supergroup (in the format @supergroupusername)
      /// </param>
      /// <param name="user_id">
      /// Unique identifier of the target user
      /// </param>
      function GetChatMember(
        Chat_id : TValue;
        User_id : Integer ) : TtgChatMember;
      /// <summary>
      /// Use this method to send answers to callback queries sent from
      /// inline keyboards. The answer will be displayed to the user as a
      /// notification at the top of the chat screen or as an alert.
      /// </summary>
      /// <returns>
      /// On success, True is returned.
      /// </returns>
      /// <remarks />
      /// <param name="callback_query_id">
      /// Unique identifier for the query to be answered
      /// </param>
      /// <param name="text">
      /// Text of the notification. If not specified, nothing will be shown
      /// to the user
      /// </param>
      /// <param name="show_alert">
      /// If true, an alert will be shown by the client instead of a
      /// notification at the top of the chat screen. Defaults to false.
      /// </param>
      function AnswerCallbackQuery(
        const Callback_query_id : string;
        const Text              : string = '';
        Show_alert              : Boolean = False ) : Boolean;
{$ENDREGION}
{$REGION 'Updating messages'}
      /// <summary>
      /// Use this method to edit text messages sent by the bot or via the
      /// bot (for inline bots).
      /// </summary>
      /// <returns>
      /// On success, if edited message is sent by the bot, the edited
      /// Message is returned, otherwise True is returned.
      /// </returns>
      /// <remarks />
      /// <param name="chat_id">
      /// Required if inline_message_id is not specified. Unique identifier
      /// for the target chat or username of the target channel (in the
      /// format @channelusername)
      /// </param>
      /// <param name="message_id">
      /// Required if inline_message_id is not specified. Unique identifier
      /// of the sent message
      /// </param>
      /// <param name="inline_message_id">
      /// Required if chat_id and message_id are not specified. Identifier of
      /// the inline message
      /// </param>
      /// <param name="text">
      /// New text of the message
      /// </param>
      /// <param name="parse_mode">
      /// Send Markdown or HTML, if you want Telegram apps to show bold,
      /// italic, fixed-width text or inline URLs in your bot's message.
      /// </param>
      /// <param name="disable_web_page_preview">
      /// Disables link previews for links in this message
      /// </param>
      /// <param name="reply_markup">
      /// A JSON-serialized object for an inline keyboard.
      /// </param>
      function EditMessageText(
        Chat_id                  : TValue;
        Message_id               : Integer;
        const Inline_message_id  : string;
        const Text               : string;
        Parse_mode               : TtgParseMode = TtgParseMode.Default;
        Disable_web_page_preview : Boolean = False;
        Reply_markup             : TtgReplyKeyboardMarkup = nil ) : Boolean;
      /// <summary>
      /// Use this method to edit captions of messages sent by the bot or via
      /// the bot (for inline bots).
      /// </summary>
      /// <returns>
      /// On success, if edited message is sent by the bot, the edited
      /// Message is returned, otherwise True is returned.
      /// </returns>
      /// <remarks />
      /// <param name="chat_id">
      /// Required if inline_message_id is not specified. Unique identifier
      /// for the target chat or username of the target channel (in the
      /// format @channelusername)
      /// </param>
      /// <param name="message_id">
      /// Required if inline_message_id is not specified. Unique identifier
      /// of the sent message
      /// </param>
      /// <param name="inline_message_id">
      /// Required if chat_id and message_id are not specified. Identifier of
      /// the inline message
      /// </param>
      /// <param name="caption">
      /// New caption of the message
      /// </param>
      /// <param name="reply_markup">
      /// A JSON-serialized object for an inline keyboard.
      /// </param>
      function EditMessageCaption(
        Chat_id                 : TValue;
        Message_id              : Integer;
        const Inline_message_id : string;
        const Caption           : string;
        Reply_markup            : TtgReplyKeyboardMarkup = nil ) : Boolean;
      /// <summary>
      /// Use this method to edit only the reply markup of messages sent by
      /// the bot or via the bot (for inline bots).
      /// </summary>
      /// <returns>
      /// On success, if edited message is sent by the bot, the edited
      /// Message is returned, otherwise True is returned.
      /// </returns>
      /// <remarks />
      /// <param name="chat_id">
      /// Required if inline_message_id is not specified. Unique identifier
      /// for the target chat or username of the target channel (in the
      /// format @channelusername)
      /// </param>
      /// <param name="message_id">
      /// Required if inline_message_id is not specified. Unique identifier
      /// of the sent message
      /// </param>
      /// <param name="inline_message_id">
      /// Required if chat_id and message_id are not specified. Identifier of
      /// the inline message
      /// </param>
      /// <param name="reply_markup">
      /// A JSON-serialized object for an inline keyboard.
      /// </param>
      function EditMessageReplyMarkup(
        Chat_id                 : TValue;
        Message_id              : Integer;
        const Inline_message_id : string;
        Reply_markup            : TtgReplyKeyboardMarkup = nil ) : Boolean;
      /// <summary>
      /// Use this method to delete a message.
      /// </summary>
      /// <param name="chat_id">
      /// Unique identifier for the target chat or username of the target
      /// channel (in the format @channelusername)
      /// </param>
      /// <param name="message_id">
      /// Identifier of the message to delete
      /// </param>
      /// <returns>
      /// Returns True on success.
      /// </returns>
      /// <remarks>
      /// A message can only be deleted if it was sent less than 48 hours
      /// ago. Any such recently sent outgoing message may be deleted.
      /// Additionally, if the bot is an administrator in a group chat, it
      /// can delete any message. If the bot is an administrator in a
      /// supergroup, it can delete messages from any other user and service
      /// messages about people joining or leaving the group (other types of
      /// service messages may only be removed by the group creator). In
      /// channels, bots can only remove their own messages.
      /// </remarks>
      function DeleteMessage(
        Chat_id    : TValue;
        Message_id : Integer ) : Boolean;
{$ENDREGION}
{$REGION 'Inline mode'}
      /// <summary>
      /// Use this method to send answers to an inline query.
      /// </summary>
      /// <returns>
      /// On success, True is returned.
      /// </returns>
      /// <remarks>
      /// No more than 50 results per query are allowed.
      /// </remarks>
      /// <param name="inline_query_id">
      /// Unique identifier for the answered query
      /// </param>
      /// <param name="results">
      /// A JSON-serialized array of results for the inline query
      /// </param>
      /// <param name="cache_time">
      /// The maximum amount of time in seconds that the result of the inline
      /// query may be cached on the server. Defaults to 300.
      /// </param>
      /// <param name="is_personal">
      /// Pass True, if results may be cached on the server side only for the
      /// user that sent the query. By default, results may be returned to
      /// any user who sends the same query
      /// </param>
      /// <param name="next_offset">
      /// Pass the offset that a client should send in the next query with
      /// the same text to receive more results. Pass an empty string if
      /// there are no more results or if you don‘t support pagination.
      /// Offset length can’t exceed 64 bytes.
      /// </param>
      /// <param name="switch_pm_text">
      /// If passed, clients will display a button with specified text that
      /// switches the user to a private chat with the bot and sends the bot
      /// a start message with the parameter switch_pm_parameter
      /// </param>
      /// <param name="switch_pm_parameter">
      /// Parameter for the start message sent to the bot when user presses
      /// the switch button
      /// </param>
      function AnswerInlineQuery(
        const Inline_query_id     : string;
        Results                   : TArray< TtgInlineQueryResult >;
        Cache_time                : Integer = 300;
        Is_personal               : Boolean = False;
        const Next_offset         : string = '';
        const Switch_pm_text      : string = '';
        const Switch_pm_parameter : string = '' ) : Boolean;
{$ENDREGION}
{$REGION 'Payments'}
      function SendInvoice(
        Chat_id               : Integer;
        const Title           : string;
        const Description     : string;
        const Payload         : string;
        const Provider_token  : string;
        const Start_parameter : string;
        const Currency        : string;
        Prices                : TArray< TtgLabeledPrice >;
        const Photo_url       : string = '';
        Photo_size            : Integer = 0;
        Photo_width           : Integer = 0;
        Photo_height          : Integer = 0;
        Need_name             : Boolean = False;
        Need_phone_number     : Boolean = False;
        Need_email            : Boolean = False;
        Need_shipping_address : Boolean = False;
        Is_flexible           : Boolean = False;
        Disable_notification  : Boolean = False;
        Reply_to_message_id   : Integer = 0;
        Reply_markup          : TtgReplyKeyboardMarkup = nil ) : TtgMessage;
      function AnswerShippingQuery(
        const Shipping_query_id : string;
        Ok                      : Boolean;
        Shipping_options        : TArray< TtgShippingOption >;
        const Error_message     : string ) : Boolean;
      function AnswerPreCheckoutQuery(
        const Pre_checkout_query_id : string;
        Ok                          : Boolean;
        const Error_message         : string = '' ) : Boolean;
{$ENDREGION}
{$REGION 'Games'}
      /// <summary>
      /// Use this method to send a game.
      /// </summary>
      /// <param name="chat_id">
      /// Unique identifier for the target chat
      /// </param>
      /// <param name="game_short_name">
      /// Short name of the game, serves as the unique identifier for the
      /// game. Set up your games via Botfather.
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
      /// A JSON-serialized object for an inline keyboard. If empty, one
      /// ‘Play game_title’ button will be shown. If not empty, the first
      /// button must launch the game.
      /// </param>
      /// <returns>
      /// On success, the sent Message is returned.
      /// </returns>
      function SendGame(
        Chat_id               : Integer;
        const Game_short_name : string;
        Disable_notification  : Boolean = False;
        Reply_to_message_id   : Integer = 0;
        Reply_markup          : TtgReplyKeyboardMarkup = nil ) : TtgMessage;
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
      /// Pass True, if the game message should not be automatically edited
      /// to include the current scoreboard
      /// </param>
      /// <param name="chat_id">
      /// Required if inline_message_id is not specified. Unique identifier
      /// for the target chat
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
      /// is not greater than the user's current score in the chat and force
      /// is False.
      /// </returns>
      function SetGameScore(
        User_id                 : Integer;
        Score                   : Integer;
        Force                   : Boolean = False;
        Disable_edit_message    : Boolean = False;
        Chat_id                 : Integer = 0;
        Message_id              : Integer = 0;
        const Inline_message_id : string = '' ) : TtgMessage;
      /// <summary>
      /// Use this method to get data for high score tables. Will return the
      /// score of the specified user and several of his neighbors in a game.
      /// </summary>
      /// <param name="user_id">
      /// Target user id
      /// </param>
      /// <param name="chat_id">
      /// Required if inline_message_id is not specified. Unique identifier
      /// for the target chat
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
      /// three users if the user and his neighbors are not among them.
      /// Please note that this behavior is subject to change.
      /// </remarks>
      function GetGameHighScores(
        User_id                 : Integer;
        Chat_id                 : Integer = 0;
        Message_id              : Integer = 0;
        const Inline_message_id : string = '' ) : TArray< TtgGameHighScore >;
{$ENDREGION}
  end;

implementation

uses
  TelegAPI.Utils,
  TelegAPI.Helpers;

{ TTelegramBotCore }

{$REGION 'Core'}

constructor TTelegramBotCore.Create( AOwner : TComponent );
  begin
    inherited Create( AOwner );
    AllowedUpdates := UPDATES_ALLOWED_ALL;
    FIsReceiving := False;
    PollingTimeout := 1000;
    MessageOffset := 0;
  end;

function TTelegramBotCore.GetVersionAPI : string;
  begin
    Result := '2.3.1';
  end;

function TTelegramBotCore.API< T >(
  const Method : string;
  Parameters   : TDictionary< string, TValue > ) : T;
  var
    LHttp : THTTPClient;
    LHttpResponse : IHTTPResponse;
    LApiResponse : TtgApiResponse< T >;
    LURL_TELEG : string;
    LParamToDate : TMultipartFormData;
  begin
    if Self.Token.IsEmpty
    then
      raise ETelegramTokenEmpty.Create( 'Token is Empty!' );
    LHttp := THTTPClient.Create;
    try
      LURL_TELEG := 'https://api.telegram.org/bot' + FToken + '/' + Method;
      // Преобразовуем параметры в строку, если нужно
      if Assigned( Parameters )
      then
      begin
        LParamToDate := ParamsToFormData( Parameters );
        LHttpResponse := LHttp.Post( LURL_TELEG, LParamToDate );
      end
      else
        LHttpResponse := LHttp.Get( LURL_TELEG );
      LApiResponse := TtgApiResponse< T >.FromJSON
        ( LHttpResponse.ContentAsString );
      if not LApiResponse.Ok
      then
      begin
        if Assigned( OnError )
        then
          OnError( TTelegramBot( Self ), LApiResponse.Code,
            LApiResponse.Message );
      end;
      Result := LApiResponse.ResultObject;
      LApiResponse.ResultObject := default ( T );
    finally
      if Assigned( Parameters )
      then
        FreeAndNil( LParamToDate );
      FreeAndNil( LHttp );
      FreeAndNil( LApiResponse );
    end;
  end;

function TTelegramBotCore.ParamsToFormData( Parameters
  : TDictionary< string, TValue > ) : TMultipartFormData;
  var
    Parameter : TPair< string, TValue >;
  begin
    Result := TMultipartFormData.Create;
    for Parameter in Parameters do
    begin
      if Parameter.Value.IsType< TtgInlineKeyboardMarkup >
      then
      begin
        { TODO -oOwner -cGeneral : Проверить че за херня тут твориться }
        if Parameter.Value.AsType< TtgInlineKeyboardMarkup > <> nil
        then
          Result.AddField( Parameter.Key,
            Parameter.Value.AsType< TtgInlineKeyboardMarkup >.AsJSON );
      end
      else if Parameter.Value.IsType< TtgReplyKeyboardMarkup >
      then
      begin
        if Parameter.Value.AsType< TtgReplyKeyboardMarkup > <> nil
        then
          Result.AddField( Parameter.Key,
            Parameter.Value.AsType< TtgReplyKeyboardMarkup >.AsJSON );
      end
      else if Parameter.Value.IsType< TtgReplyKeyboardHide >
      then
      begin
        if Parameter.Value.AsType< TtgReplyKeyboardHide > <> nil
        then
          Result.AddField( Parameter.Key,
            Parameter.Value.AsType< TtgReplyKeyboardHide >.AsJSON );
      end
      else if Parameter.Value.IsType< TtgForceReply >
      then
      begin
        if Parameter.Value.AsType< TtgForceReply > <> nil
        then
          Result.AddField( Parameter.Key,
            Parameter.Value.AsType< TtgForceReply >.AsJSON );
      end
      else if Parameter.Value.IsType< TtgFileToSend >
      then
      begin
        { TODO -oOwner -cGeneral : Отправка файлов }
        Result.AddFile( Parameter.Key,
          Parameter.Value.AsType< TtgFileToSend >.FileName );
      end
      else if Parameter.Value.IsType< string >
      then
      begin
        if not Parameter.Value.AsString.IsEmpty
        then
          Result.AddField( Parameter.Key, Parameter.Value.AsString )
      end
      else if Parameter.Value.IsType< Int64 >
      then
      begin
        if Parameter.Value.AsInt64 <> 0
        then
          Result.AddField( Parameter.Key, IntToStr( Parameter.Value.AsInt64 ) );
      end
      else if Parameter.Value.IsType< Boolean >
      then
      begin
        if Parameter.Value.AsBoolean
        then
          Result.AddField( Parameter.Key,
            TtgUtils.IfThen< string >( Parameter.Value.AsBoolean, 'true',
            'false' ) )
      end
      else
        raise ETelegramUnknownData.Create( 'Check parametr type ' +
          Parameter.Value.ToString );
    end;
  end;

procedure TTelegramBotCore.SetIsReceiving( const Value : Boolean );
  begin
    if ( CsDesigning in ComponentState )
    then
      Exit;
    FIsReceiving := Value;
    if Value
    then
    begin
      FRecesiver := TtgRecesiver.Create( True );
      FRecesiver.Bot := TTelegramBot( Self );
      FRecesiver.Start;
    end
    else
    begin
      FRecesiver.Terminate;
      FreeAndNil( FRecesiver );
    end;
  end;

function TTelegramBotCore.ArrayToString< T >( LArray : TArray< T > ) : string;
  var
    SA : ISuperArray;
    I : Integer;
    X : TtgInlineQueryResult;
  begin
    SA := TSuperArray.Create( );
    for I := low( LArray ) to high( LArray ) do
      SA.Add( T( LArray[ I ] ).AsJSONObject );
    Result := SA.AsJSON( );
  end;

destructor TTelegramBotCore.Destroy;
  begin
    Self.PollingTimeout := 0;
    if IsReceiving
    then
      IsReceiving := False;
    inherited;
  end;
{$ENDREGION}
{ TTelegram }
{$REGION 'Getting updates'}

procedure TTelegramBot.SetWebhook(
  const Url       : string;
  Certificate     : TtgFileToSend;
  Max_connections : Integer;
  Allowed_updates : TAllowedUpdates );
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'url', Url );
      Parameters.Add( 'certificate', Certificate );
      Parameters.Add( 'max_connections', Max_connections );
      Parameters.Add( 'allowed_updates', Allowed_updates.ToString );
      API< Boolean >( 'setWebhook', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.GetWebhookInfo : TtgWebhookInfo;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Result := API< TtgWebhookInfo >( 'getWebhookInfo', nil );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.GetUpdates(
  const Offset, Limit, Timeout : Integer;
  Allowed_updates              : TAllowedUpdates ) : TArray< TtgUpdate >;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'offset', Offset );
      Parameters.Add( 'limit', Limit );
      Parameters.Add( 'timeout', Timeout );
      Parameters.Add( 'allowed_updates', Allowed_updates.ToString );
      Result := API < TArray < TtgUpdate >> ( 'getUpdates', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.DeleteWebhook : Boolean;
  begin
    Result := API< Boolean >( 'deleteWebhook', nil );
  end;
{$ENDREGION}
{$REGION 'Basic methods'}

function TTelegramBot.UnbanChatMember(
  Chat_id : TValue;
  User_id : Integer ) : Boolean;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'user_id', User_id );
      Result := API< Boolean >( 'unbanChatMember', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.SendLocation(
  Chat_id              : TValue;
  Location             : TtgLocation;
  Disable_notification : Boolean;
  Reply_to_message_id  : Integer;
  Reply_markup         : TtgReplyKeyboardMarkup ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'latitude', Location.Latitude );
      Parameters.Add( 'longitude', Location.Longitude );
      Parameters.Add( 'disable_notification', Disable_notification );
      Parameters.Add( 'reply_to_message_id', Reply_to_message_id );
      Parameters.Add( 'reply_markup', Reply_markup );
      Result := API< TtgMessage >( 'sendLocation', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.SendPhoto(
  ChatId, Photo        : TValue;
  const Caption        : string;
  Disable_notification : Boolean;
  ReplyToMessageId     : Integer;
  ReplyMarkup          : TtgReplyKeyboardMarkup ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', ChatId );
      Parameters.Add( 'photo', Photo );
      Parameters.Add( 'caption', Caption );
      Parameters.Add( 'disable_notification', Disable_notification );
      Parameters.Add( 'reply_to_message_id', ReplyToMessageId );
      Parameters.Add( 'reply_markup', ReplyMarkup );
      Result := API< TtgMessage >( 'sendPhoto', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.SendSticker(
  Chat_id, Sticker     : TValue;
  const Caption        : string;
  Disable_notification : Boolean;
  Reply_to_message_id  : Integer;
  Reply_markup         : TtgReplyKeyboardMarkup ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'sticker', Sticker );
      Parameters.Add( 'caption', Caption );
      Parameters.Add( 'disable_notification', Disable_notification );
      Parameters.Add( 'reply_to_message_id', Reply_to_message_id );
      Parameters.Add( 'reply_markup', Reply_markup );
      Result := API< TtgMessage >( 'sendSticker', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.SendMessage(
  const Chat_id                               : TValue;
  const Text                                  : string;
  ParseMode                                   : TtgParseMode;
  DisableWebPagePreview, Disable_notification : Boolean;
  ReplyToMessageId                            : Integer;
  ReplyMarkup                                 : TtgReplyMarkup ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'text', Text );
      Parameters.Add( 'parse_mode', ParseMode.ToString );
      Parameters.Add( 'disable_web_page_preview', DisableWebPagePreview );
      Parameters.Add( 'disable_notification', Disable_notification );
      Parameters.Add( 'reply_to_message_id', ReplyToMessageId );
      Parameters.Add( 'reply_markup', ReplyMarkup );
      Result := API< TtgMessage >( 'sendMessage', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.SendVenue(
  Chat_id              : TValue;
  Venue                : TtgVenue;
  Disable_notification : Boolean;
  Reply_to_message_id  : Integer;
  Reply_markup         : TtgReplyKeyboardMarkup ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'latitude', Venue.Location.Latitude );
      Parameters.Add( 'longitude', Venue.Location.Longitude );
      Parameters.Add( 'title', Venue.Title );
      Parameters.Add( 'address', Venue.Address );
      Parameters.Add( 'foursquare_id', Venue.FoursquareId );
      Parameters.Add( 'disable_notification', Disable_notification );
      Parameters.Add( 'reply_to_message_id', Reply_to_message_id );
      Parameters.Add( 'reply_markup', Reply_markup );
      Result := API< TtgMessage >( 'sendVenue', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.SendVideo(
  Chat_id, Video          : TValue;
  Duration, Width, Height : Integer;
  const Caption           : string;
  Disable_notification    : Boolean;
  Reply_to_message_id     : Integer;
  Reply_markup            : TtgReplyKeyboardMarkup ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'video', Video );
      Parameters.Add( 'duration', Duration );
      Parameters.Add( 'width', Width );
      Parameters.Add( 'height', Height );
      Parameters.Add( 'caption', Caption );
      Parameters.Add( 'disable_notification', Disable_notification );
      Parameters.Add( 'reply_to_message_id', Reply_to_message_id );
      Parameters.Add( 'reply_markup', Reply_markup );
      Result := API< TtgMessage >( 'sendVideo', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.SendVideoNote(
  Chat_id, Video_note  : TValue;
  Duration, Length     : Integer;
  Disable_notification : Boolean;
  Reply_to_message_id  : Integer;
  Reply_markup         : TtgReplyKeyboardMarkup ) : TtgMessage;
  var
    LParameters : TDictionary< string, TValue >;
  begin
    LParameters := TDictionary< string, TValue >.Create;
    try
      LParameters.Add( 'chat_id', Chat_id );
      LParameters.Add( 'video_note', Video_note );
      LParameters.Add( 'duration', Duration );
      LParameters.Add( 'length', Length );
      LParameters.Add( 'disable_notification', Disable_notification );
      LParameters.Add( 'reply_to_message_id', Reply_to_message_id );
      LParameters.Add( 'reply_markup', Reply_markup );
      Result := API< TtgMessage >( 'sendVoice', LParameters );
    finally
      LParameters.Free;
    end;
  end;

function TTelegramBot.SendVoice(
  Chat_id, Voice       : TValue;
  Duration             : Integer;
  Disable_notification : Boolean;
  Reply_to_message_id  : Integer;
  Reply_markup         : TtgReplyKeyboardMarkup ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'voice', Voice );
      Parameters.Add( 'duration', Duration );
      Parameters.Add( 'disable_notification', Disable_notification );
      Parameters.Add( 'reply_to_message_id', Reply_to_message_id );
      Parameters.Add( 'reply_markup', Reply_markup );
      Result := API< TtgMessage >( 'sendVoice', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.SendAudio(
  Chat_id, Audio         : TValue;
  Duration               : Integer;
  const Performer, Title : string;
  Disable_notification   : Boolean;
  Reply_to_message_id    : Integer;
  ReplyMarkup            : TtgReplyKeyboardMarkup ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'audio', Audio );
      Parameters.Add( 'duration', Duration );
      Parameters.Add( 'performer', Performer );
      Parameters.Add( 'title', Title );
      Parameters.Add( 'disable_notification', Disable_notification );
      Parameters.Add( 'reply_to_message_id', Reply_to_message_id );
      Parameters.Add( 'reply_markup', ReplyMarkup );
      Result := API< TtgMessage >( 'sendAudio', Parameters );
    finally
      Parameters.Free;
    end;
  end;

procedure TTelegramBot.SendChatAction(
  Chat_id      : TValue;
  const Action : string );
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'action', Action );
      API< Boolean >( 'sendChatAction', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.SendContact(
  Chat_id              : TValue;
  Contact              : TtgContact;
  Disable_notification : Boolean;
  Reply_to_message_id  : Integer;
  Reply_markup         : TtgReplyKeyboardMarkup ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'phone_number', Contact.PhoneNumber );
      Parameters.Add( 'first_name', Contact.FirstName );
      Parameters.Add( 'last_name', Contact.LastName );
      Parameters.Add( 'disable_notification', Disable_notification );
      Parameters.Add( 'reply_to_message_id', Reply_to_message_id );
      Parameters.Add( 'reply_markup', Reply_markup );
      Result := API< TtgMessage >( 'sendContact', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.SendDocument(
  Chat_id, Document    : TValue;
  const Caption        : string;
  Disable_notification : Boolean;
  Reply_to_message_id  : Integer;
  Reply_markup         : TtgReplyKeyboardMarkup ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'document', Document );
      Parameters.Add( 'caption', Caption );
      Parameters.Add( 'disable_notification', Disable_notification );
      Parameters.Add( 'reply_to_message_id', Reply_to_message_id );
      Parameters.Add( 'reply_markup', Reply_markup );
      Result := API< TtgMessage >( 'sendDocument', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.KickChatMember(
  Chat_id : TValue;
  User_id : Integer ) : Boolean;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'user_id', User_id );
      Result := API< Boolean >( 'kickChatMember', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.LeaveChat( Chat_id : TValue ) : Boolean;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Result := API< Boolean >( 'leaveChat', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.GetUserProfilePhotos(
  Chat_id       : TValue;
  Offset, Limit : Integer ) : TtgUserProfilePhotos;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'offset', Offset );
      Parameters.Add( 'limit', Limit );
      Result := API< TtgUserProfilePhotos >( 'getUserProfilePhotos',
        Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.GetMe : TtgUser;
  begin
    Result := Self.API< TtgUser >( 'getMe', nil );
  end;

function TTelegramBot.ForwardMessage(
  Chat_id, From_chat_id : TValue;
  Disable_notification  : Boolean;
  Message_id            : Integer ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'from_chat_id', From_chat_id );
      Parameters.Add( 'disable_notification', Disable_notification );
      Parameters.Add( 'message_id', Message_id );
      Result := API< TtgMessage >( 'forwardMessage', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.GetChat( const Chat_id : TValue ) : TtgChat;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Result := Self.API< TtgChat >( 'getChat', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.GetChatAdministrators( const Chat_id : TValue )
  : TArray< TtgChatMember >;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Result := Self.API < TArray < TtgChatMember >> ( 'getChatAdministrators',
        Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.GetChatMember(
  Chat_id : TValue;
  User_id : Integer ) : TtgChatMember;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'user_id', User_id );
      Result := Self.API< TtgChatMember >( 'getChatMember', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.GetChatMembersCount( const Chat_id : TValue ) : Integer;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Result := Self.API< Integer >( 'getChatMembersCount', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.GetFile( const File_id : string ) : TtgFile;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'file_id', File_id );
      Result := Self.API< TtgFile >( 'getFile', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.AnswerCallbackQuery(
  const Callback_query_id, Text : string;
  Show_alert                    : Boolean ) : Boolean;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'callback_query_id', Callback_query_id );
      if not Text.IsEmpty
      then
        Parameters.Add( 'text', Text );
      if Show_alert
      then
        Parameters.Add( 'show_alert', Show_alert );
      Result := API< Boolean >( 'forwardMessage', Parameters );
    finally
      Parameters.Free;
    end;
  end;
{$ENDREGION}
{$REGION 'Updating messages'}

function TTelegramBot.EditMessageText(
  Chat_id                       : TValue;
  Message_id                    : Integer;
  const Inline_message_id, Text : string;
  Parse_mode                    : TtgParseMode;
  Disable_web_page_preview      : Boolean;
  Reply_markup                  : TtgReplyKeyboardMarkup ) : Boolean;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'message_id', Message_id );
      Parameters.Add( 'inline_message_id', Inline_message_id );
      Parameters.Add( 'text', Text );
      Parameters.Add( 'parse_mode', Parse_mode.ToString );
      Parameters.Add( 'disable_web_page_preview', Disable_web_page_preview );
      Parameters.Add( 'reply_markup', Reply_markup );
      Result := API< Boolean >( 'editMessageText', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.DeleteMessage(
  Chat_id    : TValue;
  Message_id : Integer ) : Boolean;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'message_id', Message_id );
      Result := API< Boolean >( 'deleteMessage', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.EditMessageCaption(
  Chat_id                          : TValue;
  Message_id                       : Integer;
  const Inline_message_id, Caption : string;
  Reply_markup                     : TtgReplyKeyboardMarkup ) : Boolean;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'message_id', Message_id );
      Parameters.Add( 'inline_message_id', Inline_message_id );
      Parameters.Add( 'caption', Caption );
      Parameters.Add( 'reply_markup', Reply_markup );
      Result := API< Boolean >( 'editMessageCaption', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.EditMessageReplyMarkup(
  Chat_id                 : TValue;
  Message_id              : Integer;
  const Inline_message_id : string;
  Reply_markup            : TtgReplyKeyboardMarkup ) : Boolean;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'message_id', Message_id );
      Parameters.Add( 'inline_message_id', Inline_message_id );
      Parameters.Add( 'reply_markup', Reply_markup );
      Result := API< Boolean >( 'editMessageText', Parameters );
    finally
      Parameters.Free;
    end;
  end;
{$ENDREGION}
{$REGION 'Inline mode'}

function TTelegramBot.AnswerInlineQuery(
  const Inline_query_id : string;
  Results               : TArray< TtgInlineQueryResult >;
  Cache_time            : Integer;
  Is_personal           : Boolean;
  const Next_offset, Switch_pm_text, Switch_pm_parameter : string ) : Boolean;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'inline_query_id', Inline_query_id );
      Parameters.Add( 'results', ArrayToString< TtgInlineQueryResult >
        ( Results ) );
      Parameters.Add( 'cache_time', Cache_time );
      Parameters.Add( 'is_personal', Is_personal );
      Parameters.Add( 'next_offset', Next_offset );
      Parameters.Add( 'switch_pm_text', Switch_pm_text );
      Parameters.Add( 'switch_pm_parameter', Switch_pm_parameter );
      Result := API< Boolean >( 'answerInlineQuery', Parameters );
    finally
      Parameters.Free;
    end;
  end;
{$ENDREGION}
{$REGION 'Payments'}

function TTelegramBot.SendInvoice(
  Chat_id               : Integer;
  const Title           : string;
  const Description     : string;
  const Payload         : string;
  const Provider_token  : string;
  const Start_parameter : string;
  const Currency        : string;
  Prices                : TArray< TtgLabeledPrice >;
  const Photo_url       : string;
  Photo_size            : Integer;
  Photo_width           : Integer;
  Photo_height          : Integer;
  Need_name             : Boolean;
  Need_phone_number     : Boolean;
  Need_email            : Boolean;
  Need_shipping_address : Boolean;
  Is_flexible           : Boolean;
  Disable_notification  : Boolean;
  Reply_to_message_id   : Integer;
  Reply_markup          : TtgReplyKeyboardMarkup ) : TtgMessage;
  var
    LParameters : TDictionary< string, TValue >;
  begin
    LParameters := TDictionary< string, TValue >.Create;
    try
      LParameters.Add( 'chat_id', Chat_id );
      LParameters.Add( 'title', Title );
      LParameters.Add( 'description', Description );
      LParameters.Add( 'payload', Payload );
      LParameters.Add( 'provider_token', Provider_token );
      LParameters.Add( 'currency', Currency );
      LParameters.Add( 'prices', ArrayToString< TtgLabeledPrice >( Prices ) );
      LParameters.Add( 'photo_url', Photo_url );
      LParameters.Add( 'photo_size', Photo_size );
      LParameters.Add( 'photo_width', Photo_width );
      LParameters.Add( 'photo_height', Photo_height );
      LParameters.Add( 'need_name', Need_name );
      LParameters.Add( 'need_phone_number', Need_phone_number );
      LParameters.Add( 'need_email', Need_email );
      LParameters.Add( 'need_shipping_address', Need_shipping_address );
      LParameters.Add( 'is_flexible', Is_flexible );
      LParameters.Add( 'disable_notification', Disable_notification );
      LParameters.Add( 'reply_to_message_id', Reply_to_message_id );
      LParameters.Add( 'reply_markup', Reply_markup );
      Result := API< TtgMessage >( 'sendInvoice', LParameters );
    finally
      LParameters.Free;
    end;
  end;

function TTelegramBot.AnswerPreCheckoutQuery(
  const Pre_checkout_query_id : string;
  Ok                          : Boolean;
  const Error_message         : string ) : Boolean;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'Pre_checkout_query_id', Pre_checkout_query_id );
      Parameters.Add( 'Ok', Ok );
      Parameters.Add( 'Error_message', Error_message );
      Result := API< Boolean >( 'AnswerPreCheckoutQuery', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.AnswerShippingQuery(
  const Shipping_query_id : string;
  Ok                      : Boolean;
  Shipping_options        : TArray< TtgShippingOption >;
  const Error_message     : string ) : Boolean;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'Shipping_query_id', Shipping_query_id );
      Parameters.Add( 'Ok', Ok );
      Parameters.Add( 'Shipping_options',
        ArrayToString< TtgShippingOption >( Shipping_options ) );
      Parameters.Add( 'Error_message', Error_message );
      Result := API< Boolean >( 'answerShippingQuery', Parameters );
    finally
      Parameters.Free;
    end;
  end;

{$ENDREGION}
{$REGION 'Games'}

function TTelegramBot.SetGameScore(
  User_id, Score              : Integer;
  Force, Disable_edit_message : Boolean;
  Chat_id, Message_id         : Integer;
  const Inline_message_id     : string ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'user_id', User_id );
      Parameters.Add( 'score', Score );
      Parameters.Add( 'force', Force );
      Parameters.Add( 'disable_edit_message', Disable_edit_message );
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'message_id', Message_id );
      Parameters.Add( 'inline_message_id', Inline_message_id );
      Result := API< TtgMessage >( 'setGameScore', Parameters );
    finally
      Parameters.Free;
    end;
  end;

function TTelegramBot.SendGame(
  Chat_id               : Integer;
  const Game_short_name : string;
  Disable_notification  : Boolean;
  Reply_to_message_id   : Integer;
  Reply_markup          : TtgReplyKeyboardMarkup ) : TtgMessage;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'game_short_name', Game_short_name );
      Parameters.Add( 'disable_notification', Disable_notification );
      Parameters.Add( 'reply_to_message_id', Reply_to_message_id );
      Parameters.Add( 'reply_markup', Reply_markup );
      Result := API< TtgMessage >( 'sendGame', Parameters );
    finally
      Parameters.Free;
    end;

  end;

function TTelegramBot.GetGameHighScores(
  User_id, Chat_id, Message_id : Integer;
  const Inline_message_id      : string ) : TArray< TtgGameHighScore >;
  var
    Parameters : TDictionary< string, TValue >;
  begin
    Parameters := TDictionary< string, TValue >.Create;
    try
      Parameters.Add( 'user_id', User_id );
      Parameters.Add( 'chat_id', Chat_id );
      Parameters.Add( 'message_id', Message_id );
      Parameters.Add( 'inline_message_id', Inline_message_id );
      Result := API < TArray < TtgGameHighScore >> ( 'getGameHighScores',
        Parameters );
    finally
      Parameters.Free;
    end;
  end;

{$ENDREGION}
{$REGION 'Async'}

procedure TtgRecesiver.Execute;
  var
    LUpdates : TArray< TtgUpdate >;
  begin
    repeat
      Sleep( Bot.PollingTimeout );
      if ( Terminated ) or ( not Bot.IsReceiving )
      then
        Break;
      LUpdates := FBot.GetUpdates( Bot.MessageOffset, 100, 0,
        UPDATES_ALLOWED_ALL );
      if Length( LUpdates ) = 0
      then
        Continue;
      Bot.MessageOffset := LUpdates[ high( LUpdates ) ].Id + 1;
{$IFDEF NO_QUEUE}
{$ELSE}
      TThread.Queue( Self,
        procedure
          var
            I : Integer;
          begin
{$ENDIF}
            if Assigned( Bot.OnUpdates ) and Assigned( LUpdates )
            then
              Bot.OnUpdates( Bot, LUpdates );
            if Assigned( LUpdates )
            then
            begin
              for I := low( LUpdates ) to high( LUpdates ) do
                FreeAndNil( LUpdates[ I ] );
              LUpdates := nil;
            end;
{$IFDEF NO_QUEUE}
{$ELSE}
          end );
{$ENDIF}
    until ( Terminated ) or ( not Bot.IsReceiving );
  end;
{$ENDREGION}

end.
