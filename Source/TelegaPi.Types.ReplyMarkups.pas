unit TelegaPi.Types.ReplyMarkups;

interface

uses
  System.Generics.Collections,
  TelegaPi.Types,
  DJSON.Attributes;

type
  /// <summary>
  ///   Objects implementing this Interface define how a <see cref="TelegAPi.Types|TtgUser" />
  ///    can reply to the sent <see cref="TelegAPi.Types|TtgMessage" />
  /// </summary>
  IReplyMarkup = interface
    ['{4DCF23BA-8A37-46EF-A832-F325532B509A}']
  end;

  /// <summary>
  ///   Defines how clients display a reply intreface to the user
  /// </summary>
  /// <seealso cref="Telegram.Bot.Types.ReplyMarkups.IReplyMarkup" />
  TtgReplyMarkup = class abstract(TInterfacedObject, IReplyMarkup)
  public
    /// <summary>
    ///   Optional. Use this parameter if you want to show the keyboard to
    ///   specific users only. Targets: 1) users that are @mentioned in the
    ///   text of the Message object; 2) if the bot's message is a reply (has <see cref="Message.ReplyToMessage" />
    ///    ), sender of the original message. Example: A user requests to
    ///   change the bot's language, bot replies to the request with a keyboard
    ///   to select the new language. Other users in the group don't see the
    ///   keyboard.
    /// </summary>
    [djName('selective')]
    Selective: Boolean;
  end;

  /// <summary>
  ///   Upon receiving a <see cref="TelegAPi.Types|TtgMessage" /> with this
  ///   object, Telegram clients will display a reply interface to the user
  ///   (act as if the user has selected the bot's message and tapped 'Reply').
  ///   This can be extremely useful if you want to create user-friendly
  ///   step-by-step interfaces without having to sacrifice privacy mode.
  /// </summary>
  TtgForceReply = class(TtgReplyMarkup)
  public
    /// <summary>
    ///   Shows reply interface to the user, as if they manually selected the
    ///   bot‘s message and tapped ’Reply'
    /// </summary>
    [djName('force_reply')]
    Force: Boolean;
  end;

  TtgButtonedMarkup<T: class> = class(TInterfacedObject, IReplyMarkup)
  private
    FKeyboard: TObjectList<TObjectList<T>>;
    function GetKeyboard: TArray<TArray<T>>;
    procedure SetKeyboard(Value: TArray<TArray<T>>);
  public
    constructor Create; overload;
    procedure AddRow(AKeyboardRow: TArray<T>);
    destructor Destroy; override;
    /// <summary>
    ///   Array of <see cref="InlineKeyboardButton" /> rows, each represented
    ///   by an Array of <see cref="InlineKeyboardButton" />.
    /// </summary>
    property Keyboard: TArray<TArray<T>> read GetKeyboard write SetKeyboard;
    property KeyboardList: TObjectList<TObjectList<T>> read FKeyboard write FKeyboard;
  end;

  TtgButtonedReplyMarkup<T: class> = class(TtgButtonedMarkup<T>)
  public
    /// <summary>
    ///   <para>
    ///     Optional. Use this parameter if you want to show the keyboard to
    ///     specific users only.
    ///   </para>
    ///   <para>
    ///     Targets:
    ///   </para>
    ///   <list type="number">
    ///     <item>
    ///       users that are @mentioned in the text of the Message object;
    ///     </item>
    ///     <item>
    ///       if the bot's message is a reply (has <see cref="Message.ReplyToMessage" />
    ///        ), sender of the original message.
    ///     </item>
    ///   </list>
    ///    Example: A user requests to change the bot's language, bot replies
    ///   to the request with a keyboard to select the new language. Other
    ///   users in the group don't see the keyboard.
    /// </summary>
    [djName('selective')]
    Selective: Boolean;
  end;

  /// <summary>
  ///   This object represents an inline keyboard that appears right next to
  ///   the <see cref="TelegAPi.Types|TtgMessage" /> it belongs to.
  /// </summary>
  /// <remarks>
  ///   Inline keyboards are currently being tested and are not available in
  ///   channels yet. For now, feel free to use them in one-on-one chats or
  ///   groups.
  /// </remarks>
  TtgInlineKeyboardMarkup = class(TtgButtonedMarkup<TtgInlineKeyboardButton>)
  public
    /// <summary>
    ///   Initializes a new instance of the <see cref="InlineKeyboardMarkup" />
    ///   class.
    /// </summary>
    /// <param name="AInlineKeyboardRow">
    ///   The inline keyboard row.
    /// </param>
    constructor Create(AInlineKeyboardRow: TArray<TtgInlineKeyboardButton>); overload;
    /// <summary>
    ///   Initializes a new instance of the <see cref="InlineKeyboardMarkup" />
    ///   class.
    /// </summary>
    /// <param name="AInlineKeyboard">
    ///   The inline keyboard.
    /// </param>
    constructor Create(AInlineKeyboard: TArray<TArray<TtgInlineKeyboardButton>>); overload;
    /// <summary>
    ///   Array of <see cref="TelegAPi.Types|TtgInlineKeyboardButton" /> rows,
    ///   each represented by an Array of <see cref="TelegAPi.Types|TtgInlineKeyboardButton" />
    ///    .
    /// </summary>
    [djName('inline_keyboard')]
    property Keyboard;
  end;

  /// <summary>
  ///   This object represents a <see href="https://core.telegram.org/bots#keyboards">
  ///   custom keyboard</see> with reply options (see <see cref="https://core.telegram.org/bots#keyboards">
  ///   Introduction to bots</see> for details and examples).
  /// </summary>
  TtgReplyKeyboardMarkup = class(TtgButtonedReplyMarkup<TtgKeyboardButton>)
  public
    /// <summary>
    ///   Optional. Requests clients to resize the keyboard vertically for
    ///   optimal fit (e.g., make the keyboard smaller if there are just two
    ///   rows of <see cref="TelegAPi.Types|TtgKeyboardButton" />). Defaults to
    ///   <c>false</c>, in which case the custom keyboard is always of the same
    ///   height as the app's standard keyboard.
    /// </summary>
    [djName('resize_keyboard')]
    ResizeKeyboard: Boolean;
    /// <summary>
    ///   Optional. Requests clients to hide the keyboard as soon as it's been
    ///   used. Defaults to <c>false</c>.
    /// </summary>
    [djName('one_time_keyboard')]
    OneTimeKeyboard: Boolean;
    constructor Create(AResizeKeyboard, AOneTimeKeyboard: Boolean); overload;
    /// <summary>
    ///   Initializes a new instance of the <see cref="ReplyKeyboardMarkup" />
    ///   class.
    /// </summary>
    /// <param name="AKeyboardRow">
    ///   The keyboard row.
    /// </param>
    /// <param name="AResizeKeyboard">
    ///   if set to <c>true</c> the keyboard resizes vertically for optimal
    ///   fit.
    /// </param>
    /// <param name="AOneTimeKeyboard">
    ///   if set to <c>true</c> the client hides the keyboard as soon as it's
    ///   been used.
    /// </param>
    constructor Create(AKeyboardRow: TArray<TtgKeyboardButton>; AResizeKeyboard: Boolean = False; AOneTimeKeyboard: Boolean = False); overload;

    /// <summary>
    ///   Initializes a new instance of the <see cref="ReplyKeyboardMarkup" />
    ///   class.
    /// </summary>
    /// <param name="AResizeKeyboard">
    ///   if set to true the keyboard resizes vertically for optimal fit. <br />
    /// </param>
    /// <param name="AOneTimeKeyboard">
    ///   if set to true the client hides the keyboard as soon as it's been
    ///   used. <br />
    /// </param>
    constructor Create(AKeyboard: TArray<TArray<TtgKeyboardButton>>; AResizeKeyboard: Boolean = False; AOneTimeKeyboard: Boolean = False); overload;
    /// <summary>
    ///   Array of button rows, each represented by an Array of <see cref="TelegAPi.Types|TtgKeyboardButton">
    ///   KeyboardButton</see> objects
    /// </summary>
    [djName('keyboard')]
    property Keyboard;
  end;

  /// <summary>
  ///   Upon receiving a message with this object, Telegram clients will remove
  ///   the current custom keyboard and display the default letter-keyboard. By
  ///   default, custom keyboards are displayed until a new keyboard is sent by
  ///   a bot. An exception is made for one-time keyboards that are hidden
  ///   immediately after the user presses a button
  /// </summary>
  TtgReplyKeyboardRemove = class(TtgReplyMarkup)
  public
    /// <summary>
    ///   Requests clients to remove the custom keyboard (user will not be able
    ///   to summon this keyboard; if you want to hide the keyboard from sight
    ///   but keep it accessible, use <see cref="TelegaPi.Types.ReplyMarkups|TtgReplyKeyboardMarkup.OneTimeKeyboard">
    ///   one_time_keyboard</see> in <see cref="TelegaPi.Types.ReplyMarkups|TtgReplyKeyboardMarkup">
    ///   ReplyKeyboardMarkup</see>)
    /// </summary>
    [djName('remove_keyboard')]
    RemoveKeyboard: Boolean;
    constructor Create(ARemoveKeyboard: Boolean = True);
  end;

implementation

uses
  System.SysUtils;

{TtgButtonedMarkup<T>}

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
  FKeyboard := TObjectList<TObjectList<T>>.Create;
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

procedure TtgButtonedMarkup<T>.SetKeyboard(Value: TArray<TArray<T>>);
var
  i: Integer;
begin
  FKeyboard.Clear;
  for i := Low(Value) to High(Value) do
    Self.AddRow(Value[i]);
end;

{ TtgInlineKeyboardMarkup }

constructor TtgInlineKeyboardMarkup.Create(AInlineKeyboardRow: TArray<TtgInlineKeyboardButton>);
begin
  inherited Create;
  Self.AddRow(AInlineKeyboardRow);
end;

constructor TtgInlineKeyboardMarkup.Create(AInlineKeyboard: TArray<TArray<TtgInlineKeyboardButton>>);
var
  i: Integer;
begin
  inherited Create;
  for i := Low(AInlineKeyboard) to High(AInlineKeyboard) do
  begin
    AddRow(AInlineKeyboard[i]);
  end;
end;

{ TtgReplyKeyboardMarkup }

constructor TtgReplyKeyboardMarkup.Create(AKeyboardRow: TArray<TtgKeyboardButton>; AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  inherited Create;
  AddRow(AKeyboardRow);
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

constructor TtgReplyKeyboardMarkup.Create(AKeyboard: TArray<TArray<TtgKeyboardButton>>; AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  inherited Create;
  Self.Keyboard := AKeyboard;
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

constructor TtgReplyKeyboardMarkup.Create(AResizeKeyboard, AOneTimeKeyboard: Boolean);
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

