unit TelegaPi.Types.ReplyMarkups;

interface

uses
  System.Generics.Collections,
  REST.Json.Types;

type
  /// <summary>
  /// This object represents one button of an inline keyboard. You must use
  /// exactly one of the optional fields.
  /// </summary>
  TtgInlineKeyboardButton = class
  private
    FText: string;
    FCallbackData: string;
    FPay: Boolean;
    FURL: string;
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="TelegAPi.Types|TtgKeyboardButton" />
    /// class.
    /// </summary>
    /// <param name="AText">
    /// Text of the button
    /// </param>
    constructor Create(const AText: string); overload;
    /// <summary>
    /// Initializes a new instance of the <see cref="TelegAPi.Types|TtgKeyboardButton" />
    /// class.
    /// </summary>
    /// <param name="AText">
    /// The text. <br />
    /// </param>
    /// <param name="ACallbackData">
    /// The callback data. <br />
    /// </param>
    constructor Create(const AText, ACallbackData: string); overload;
    /// <summary>
    /// Optional. HTTP url to be opened when button is pressed
    /// </summary>
    [JSONName('url')]
    property Url: string read FURL write FURL;
    /// <summary>
    /// Optional. Data to be sent in a callback query to the bot when button
    /// is pressed, 1-64 bytes
    /// </summary>
    [JSONName('callback_data')]
    property CallbackData: string read FCallbackData write FCallbackData;
    /// <summary>
    /// Optional. If set, pressing the button will prompt the user to select
    /// one of their chats, open that chat and insert the bot‘s username and
    /// the specified inline query in the input field. Can be empty, in which
    /// case just the bot’s username will be inserted.
    /// </summary>
    /// <remarks>
    /// Note: This offers an easy way for users to start using your bot in
    /// inline mode when they are currently in a private chat with it.
    /// Especially useful when combined with switch_pm… actions – in this
    /// case the user will be automatically returned to the chat they
    /// switched from, skipping the chat selection screen.
    /// </remarks>
    [JSONName('switch_inline_query')]
    property SwitchInlineQuery: string read FCallbackData write FCallbackData;
    /// <summary>
    /// Optional. If set, pressing the button will insert the bot‘s username
    /// and the specified inline query in the current chat's input field. Can
    /// be empty, in which case only the bot’s username will be inserted. <br /><br />
    /// This offers a quick way for the user to open your bot in inline mode
    /// in the same chat – good for selecting something from multiple
    /// options.
    /// </summary>
    [JSONName('switch_inline_query_current_chat')]
    property SwitchInlineQueryCurrentChat: string read FCallbackData
      write FCallbackData;
    /// <summary>
    /// Optional. Description of the game that will be launched when the user
    /// presses the button. <br /><br />
    /// </summary>
    /// <remarks>
    /// NOTE: This type of button must always be the first button in the
    /// first row.
    /// </remarks>
    [JSONName('callback_game')]
    property CallbackGame: string read FCallbackData write FCallbackData;
    /// <summary>
    /// Optional. Specify True, to send a Pay button. <br /><br />
    /// </summary>
    /// <remarks>
    /// NOTE: This type of button must always be the first button in the
    /// first row.
    /// </remarks>
    [JSONName('pay')]
    property Pay: Boolean read FPay write FPay;

    /// <summary>
    /// Label text on the button
    /// </summary>
    [JSONName('text')]
    property Text: string read FText write FText;
  end;
  /// <summary>
  /// This object represents one button of the reply keyboard. For simple
  /// text buttons String can be used instead of this object to specify text
  /// of the button. Optional fields are mutually exclusive.
  /// </summary>
  /// <remarks>
  /// request_contact and request_location options will only work in Telegram
  /// versions released after 9 April, 2016. Older clients will ignore them.
  /// </remarks>

  TtgKeyboardButton = class(TObject)
  private
    FRequestLocation: Boolean;
    FText: string;
    FRequestContact: Boolean;
  public
    constructor Create(const AText: string; ARequestContact: Boolean = False;
      ARequestLocation: Boolean = False); overload;
    /// <summary>
    /// Text of the button. If none of the optional fields are used, it will
    /// be sent to the bot as a message when the button is pressed
    /// </summary>
    [JSONName('text')]
    property Text: string read FText write FText;
    /// <summary>
    /// Optional. If True, the user's phone number will be sent as a contact
    /// when the button is pressed. Available in private chats only
    /// </summary>
    [JSONName('request_contact')]
    property RequestContact: Boolean read FRequestContact write FRequestContact;
    /// <summary>
    /// Optional. If True, the user's current location will be sent when the
    /// button is pressed. Available in private chats only
    /// </summary>
    [JSONName('request_location')]
    property RequestLocation: Boolean read FRequestLocation
      write FRequestLocation;
  end;

  /// <summary>
  /// Objects implementing this Interface define how a <see cref="TelegAPi.Types|TtgUser" />
  /// can reply to the sent <see cref="TelegAPi.Types|TtgMessage" />
  /// </summary>
  IReplyMarkup = interface
    ['{4DCF23BA-8A37-46EF-A832-F325532B509A}']
  end;

  /// <summary>
  /// Defines how clients display a reply intreface to the user
  /// </summary>
  /// <seealso cref="Telegram.Bot.Types.ReplyMarkups.IReplyMarkup" />
  TtgReplyMarkup = class abstract(TInterfacedObject, IReplyMarkup)
  private
    FSelective: Boolean;
  public
    /// <summary>
    /// Optional. Use this parameter if you want to show the keyboard to
    /// specific users only. Targets: 1) users that are @mentioned in the
    /// text of the Message object; 2) if the bot's message is a reply (has <see cref="Message.ReplyToMessage" />
    /// ), sender of the original message. Example: A user requests to
    /// change the bot's language, bot replies to the request with a keyboard
    /// to select the new language. Other users in the group don't see the
    /// keyboard.
    /// </summary>
    [JSONName('selective')]
    property Selective: Boolean read FSelective write FSelective;
  end;

  /// <summary>
  /// Upon receiving a <see cref="TelegAPi.Types|TtgMessage" /> with this
  /// object, Telegram clients will display a reply interface to the user
  /// (act as if the user has selected the bot's message and tapped 'Reply').
  /// This can be extremely useful if you want to create user-friendly
  /// step-by-step interfaces without having to sacrifice privacy mode.
  /// </summary>
  TtgForceReply = class(TtgReplyMarkup)
  private
    FForce: Boolean;
  public
    /// <summary>
    /// Shows reply interface to the user, as if they manually selected the
    /// bot‘s message and tapped ’Reply'
    /// </summary>
    [JSONName('force_reply')]
    property Force: Boolean read FForce write FForce;
  end;

  TtgButtonedMarkup<T: class> = class(TInterfacedObject, IReplyMarkup)
  private
    FKeyboard: TObjectList<TObjectList<T>>;
    function GetKeyboard: TArray<TArray<T>>;
    procedure SetKeyboard(Value: TArray < TArray < T >> );
  public
    constructor Create; overload;
    procedure AddRow(AKeyboardRow: TArray<T>);
    destructor Destroy; override;
    /// <summary>
    /// Array of <see cref="InlineKeyboardButton" /> rows, each represented
    /// by an Array of <see cref="InlineKeyboardButton" />.
    /// </summary>
    [JsonMarshalled(False)]
    property Keyboard: TArray < TArray < T >> read GetKeyboard
      write SetKeyboard;
    [JsonMarshalled(False)]
    property KeyboardList: TObjectList < TObjectList < T >> read FKeyboard
      write FKeyboard;
  end;

  TtgButtonedReplyMarkup<T: class> = class(TtgButtonedMarkup<T>)
  private
    FSelective: Boolean;
  public
    /// <summary>
    /// <para>
    /// Optional. Use this parameter if you want to show the keyboard to
    /// specific users only.
    /// </para>
    /// <para>
    /// Targets:
    /// </para>
    /// <list type="number">
    /// <item>
    /// users that are @mentioned in the text of the Message object;
    /// </item>
    /// <item>
    /// if the bot's message is a reply (has <see cref="Message.ReplyToMessage" />
    /// ), sender of the original message.
    /// </item>
    /// </list>
    /// Example: A user requests to change the bot's language, bot replies
    /// to the request with a keyboard to select the new language. Other
    /// users in the group don't see the keyboard.
    /// </summary>
    [JSONName('selective')]
    property Selective: Boolean read FSelective write FSelective;
  end;

  /// <summary>
  /// This object represents an inline keyboard that appears right next to
  /// the <see cref="TelegAPi.Types|TtgMessage" /> it belongs to.
  /// </summary>
  /// <remarks>
  /// Inline keyboards are currently being tested and are not available in
  /// channels yet. For now, feel free to use them in one-on-one chats or
  /// groups.
  /// </remarks>
  TtgInlineKeyboardMarkup = class(TtgButtonedMarkup<TtgInlineKeyboardButton>)
  public
    constructor Create; overload;
    /// <summary>
    /// Initializes a new instance of the <see cref="InlineKeyboardMarkup" />
    /// class.
    /// </summary>
    /// <param name="AInlineKeyboardRow">
    /// The inline keyboard row.
    /// </param>
    constructor Create(AInlineKeyboardRow
      : TArray<TtgInlineKeyboardButton>); overload;
    /// <summary>
    /// Initializes a new instance of the <see cref="InlineKeyboardMarkup" />
    /// class.
    /// </summary>
    /// <param name="AInlineKeyboard">
    /// The inline keyboard.
    /// </param>
    constructor Create(AInlineKeyboard: TArray < TArray <
      TtgInlineKeyboardButton >> ); overload;
    /// <summary>
    /// Array of <see cref="TelegAPi.Types|TtgInlineKeyboardButton" /> rows,
    /// each represented by an Array of <see cref="TelegAPi.Types|TtgInlineKeyboardButton" />
    /// .
    /// </summary>
    [JSONName('inline_keyboard')]
    property Keyboard;
  end;

  /// <summary>
  /// This object represents a <see href="https://core.telegram.org/bots#keyboards">
  /// custom keyboard</see> with reply options (see <see cref="https://core.telegram.org/bots#keyboards">
  /// Introduction to bots</see> for details and examples).
  /// </summary>
  TtgReplyKeyboardMarkup = class(TtgButtonedReplyMarkup<TtgKeyboardButton>)
  private
    FResizeKeyboard: Boolean;
    FOneTimeKeyboard: Boolean;
  public
    constructor Create(AResizeKeyboard, AOneTimeKeyboard: Boolean); overload;
    /// <summary>
    /// Initializes a new instance of the <see cref="ReplyKeyboardMarkup" />
    /// class.
    /// </summary>
    /// <param name="AKeyboardRow">
    /// The keyboard row.
    /// </param>
    /// <param name="AResizeKeyboard">
    /// if set to <c>true</c> the keyboard resizes vertically for optimal
    /// fit.
    /// </param>
    /// <param name="AOneTimeKeyboard">
    /// if set to <c>true</c> the client hides the keyboard as soon as it's
    /// been used.
    /// </param>
    constructor Create(AKeyboardRow: TArray<TtgKeyboardButton>;
      AResizeKeyboard: Boolean = False;
      AOneTimeKeyboard: Boolean = False); overload;

    /// <summary>
    /// Initializes a new instance of the <see cref="ReplyKeyboardMarkup" />
    /// class.
    /// </summary>
    /// <param name="AResizeKeyboard">
    /// if set to true the keyboard resizes vertically for optimal fit. <br />
    /// </param>
    /// <param name="AOneTimeKeyboard">
    /// if set to true the client hides the keyboard as soon as it's been
    /// used. <br />
    /// </param>
    constructor Create(AKeyboard: TArray<TArray<TtgKeyboardButton>>;
      AResizeKeyboard: Boolean = False;
      AOneTimeKeyboard: Boolean = False); overload;
    /// <summary>
    /// Array of button rows, each represented by an Array of <see cref="TelegAPi.Types|TtgKeyboardButton">
    /// KeyboardButton</see> objects
    /// </summary>
    [JSONName('keyboard')]
    property Keyboard;
    /// <summary>
    /// Optional. Requests clients to hide the keyboard as soon as it's been
    /// used. Defaults to <c>false</c>.
    /// </summary>
    [JSONName('one_time_keyboard')]
    property OneTimeKeyboard: Boolean read FOneTimeKeyboard
      write FOneTimeKeyboard;
    /// <summary>
    /// Optional. Requests clients to resize the keyboard vertically for
    /// optimal fit (e.g., make the keyboard smaller if there are just two
    /// rows of <see cref="TelegAPi.Types|TtgKeyboardButton" />). Defaults to
    /// <c>false</c>, in which case the custom keyboard is always of the same
    /// height as the app's standard keyboard.
    /// </summary>
    [JSONName('resize_keyboard')]
    property ResizeKeyboard: Boolean read FResizeKeyboard write FResizeKeyboard;
  end;

  /// <summary>
  /// Upon receiving a message with this object, Telegram clients will remove
  /// the current custom keyboard and display the default letter-keyboard. By
  /// default, custom keyboards are displayed until a new keyboard is sent by
  /// a bot. An exception is made for one-time keyboards that are hidden
  /// immediately after the user presses a button
  /// </summary>
  TtgReplyKeyboardRemove = class(TtgReplyMarkup)
  private
    FRemoveKeyboard: Boolean;
  public
    constructor Create(ARemoveKeyboard: Boolean = True);
    /// <summary>
    /// Requests clients to remove the custom keyboard (user will not be able
    /// to summon this keyboard; if you want to hide the keyboard from sight
    /// but keep it accessible, use <see cref="TelegaPi.Types.ReplyMarkups|TtgReplyKeyboardMarkup.OneTimeKeyboard">
    /// one_time_keyboard</see> in <see cref="TelegaPi.Types.ReplyMarkups|TtgReplyKeyboardMarkup">
    /// ReplyKeyboardMarkup</see>)
    /// </summary>
    [JSONName('remove_keyboard')]
    property RemoveKeyboard: Boolean read FRemoveKeyboard write FRemoveKeyboard;
  end;

implementation

uses
  System.SysUtils;
{ TtgInlineKeyboardButton }

constructor TtgInlineKeyboardButton.Create(const AText: string);
begin
  Text := AText;
end;

constructor TtgInlineKeyboardButton.Create(const AText, ACallbackData: string);
begin
  Self.Create(AText);
  Self.CallbackData := ACallbackData;
end;

{ TtgKeyboardButton }

constructor TtgKeyboardButton.Create(const AText: string;
  ARequestContact, ARequestLocation: Boolean);
begin
  Self.Text := AText;
  Self.RequestContact := ARequestContact;
  Self.RequestLocation := ARequestLocation;
end;
{ TtgButtonedMarkup<T> }

procedure TtgButtonedMarkup<T>.AddRow(AKeyboardRow: TArray<T>);
var
  LListBtn: TObjectList<T>;
begin
  LListBtn := TObjectList<T>.Create;
  LListBtn.AddRange(AKeyboardRow);
  FKeyboard.Add(LListBtn);
end;

constructor TtgButtonedMarkup<T>.Create;
begin
  inherited;
  FKeyboard := TObjectList < TObjectList < T >>.Create;
end;

destructor TtgButtonedMarkup<T>.Destroy;
begin
  FreeAndNil(FKeyboard);
  inherited;
end;

function TtgButtonedMarkup<T>.GetKeyboard: TArray<TArray<T>>;
var
  i: Integer;
begin
  SetLength(Result, FKeyboard.Count);
  for i := 0 to FKeyboard.Count - 1 do
  begin
    Result[i] := FKeyboard[i].ToArray;
  end;
end;

procedure TtgButtonedMarkup<T>.SetKeyboard(Value: TArray < TArray < T >> );
var
  i: Integer;
begin
  FKeyboard.Clear;
  for i := Low(Value) to High(Value) do
    Self.AddRow(Value[i]);
end;

{ TtgInlineKeyboardMarkup }

constructor TtgInlineKeyboardMarkup.Create(AInlineKeyboardRow
  : TArray<TtgInlineKeyboardButton>);
begin
  inherited Create;
  Self.AddRow(AInlineKeyboardRow);
end;

constructor TtgInlineKeyboardMarkup.Create(AInlineKeyboard: TArray < TArray <
  TtgInlineKeyboardButton >> );
var
  i: Integer;
begin
  Self.Create;
  for i := Low(AInlineKeyboard) to High(AInlineKeyboard) do
  begin
    AddRow(AInlineKeyboard[i]);
  end;
end;

constructor TtgInlineKeyboardMarkup.Create;
begin
  inherited Create;
end;

{ TtgReplyKeyboardMarkup }

constructor TtgReplyKeyboardMarkup.Create(AKeyboardRow
  : TArray<TtgKeyboardButton>; AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  inherited Create;
  AddRow(AKeyboardRow);
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

constructor TtgReplyKeyboardMarkup.Create
  (AKeyboard: TArray<TArray<TtgKeyboardButton>>;
  AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  inherited Create;
  Self.Keyboard := AKeyboard;
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

constructor TtgReplyKeyboardMarkup.Create(AResizeKeyboard, AOneTimeKeyboard
  : Boolean);
begin
  inherited Create;
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

{ TtgReplyKeyboardRemove }

constructor TtgReplyKeyboardRemove.Create(ARemoveKeyboard: Boolean);
begin
  inherited Create;
  RemoveKeyboard := ARemoveKeyboard;
end;

end.
