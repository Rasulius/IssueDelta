unit searchforms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LazUTF8, syncobjs, DateUtils;

type
  { TSearchForm }
  TSearchStatus = (ssFind, ssNotFind, ssTimout);
  TTextSearchThread = class;
  TShowStatusEvent = procedure(FindIndex: Integer; Length: Integer; SearchStatus: TSearchStatus) of Object;

  TSearchForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FTextSearcher: TTextSearchThread;
    FText: TStrings;
    FFindIndex: Integer;
    FEvent: TEvent;
  public
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;

    procedure OnFindText(FindIndex: Integer; Length: Integer; SearchStatus: TSearchStatus);
  end;

type

  { TTextSearchThread }


  TTextSearchThread = class (TThread)
  private
    FBufferSize: Integer;
    FFindText: String;
    FText: String;
    FTextLength: Integer;
    FOnShowStatus: TShowStatusEvent;
    FFindIndex: Integer;
    FEvent: TEvent;
    FWaitingTime: Integer;
    FSearchStatus: TSearchStatus;
    FPortionIndex: Integer;
  protected
    procedure Execute; override;
    procedure ShowStatus();
  public
    constructor Create(CreateSuspendedThread: Boolean; anEvent: TEvent; WaitingTime: Integer = 100);
    procedure Init(const FindText: String; AllText: TStrings; Offset: Integer = 1);

    destructor Destroy; override;

    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;



procedure ShowSearchForm(Owner: TComponent; aText: TStrings);

var
  SearchForm: TSearchForm;
  FormCount: Integer;


implementation

{$R *.lfm}

procedure ShowSearchForm(Owner: TComponent;  aText: TStrings);
var
  SearchForm: TSearchForm;
begin
  SearchForm := TSearchForm.Create(Owner);
  try
    SearchForm.Name := 'MyForm' + IntToStr(FormCount);
    Inc(FormCount);
    SearchForm.Show;
  except
    SearchForm.Free;
  end;
end;

{ TTextSearchThread }

constructor TTextSearchThread.Create(CreateSuspendedThread: Boolean; anEvent: TEvent; WaitingTime: Integer = 100);
begin
  inherited Create(CreateSuspendedThread);
  FTextLength := 0;
  FFindIndex := 1;
  FreeOnTerminate := True;
  FEvent := anEvent;
  FWaitingTime := WaitingTime;
  FBufferSize:= 50;
end;

procedure TTextSearchThread.Init(const FindText: String; AllText: TStrings; Offset: Integer);
begin
  FFindText := FindText;
  FFindIndex := Offset;
  FText := AllText.Text;
  FTextLength:= Utf8Length(FText);
end;

destructor TTextSearchThread.Destroy;
begin
  Terminate;
  inherited Destroy;
end;

procedure TTextSearchThread.Execute;
var
  Portion: String;
  StopSearch: Boolean;
  StartIndex: Integer;
  BufferLength: Integer;
  PortionSize: Integer;
  StartTime: TDateTime;
  DiffTime: Extended;

begin
  FPortionIndex := 0;
  PortionSize := FBufferSize;
  FSearchStatus := ssNotFind;

  while not Terminated do begin
    FEvent.WaitFor(Infinite);
    StartTime := Now;
    StopSearch := False;
    StartIndex := 0;
    PortionSize := FBufferSize;

    while not StopSearch do begin
      if ((StartIndex + PortionSize) > FTextLength) then begin
        PortionSize := FTextLength - StartIndex;
        StopSearch := True;
      end;
      { timeout }
      DiffTime:=  MillisecondOfTheDay(StartTime-Now);

      //if(DiffTime> FWaitingTime) then begin
      //  StopSearch := True;
      //  FSearchStatus := ssTimout;
      //  Synchronize(@ShowStatus);
      //end;


      Portion := Copy(FText, StartIndex, PortionSize);
      if FFindIndex = 0 then
        FFindIndex := 1

      FFindIndex := Utf8Pos(FFindText, Portion, FFindIndex);
      { find }
      if FFindIndex > 0 then begin
        FSearchStatus := ssFind;
        Synchronize(@ShowStatus);

      end;
      StartIndex := StartIndex + PortionSize;
      Inc(FPortionIndex);
    end;
    FEvent.ResetEvent;

    if FSearchStatus = ssNotFind then
      Synchronize(@ShowStatus);
  end;
end;

procedure TTextSearchThread.ShowStatus();
begin
  if Assigned(FOnShowStatus) then begin
    FOnShowStatus(FFindIndex-1+FPortionIndex*FBufferSize, Utf8Length(FFindText), FSearchStatus);
  end;
end;

{ TSearchForm }

procedure TSearchForm.Button1Click(Sender: TObject);
begin
  FTextSearcher.Init(Edit1.Text, Memo1.Lines);
  FEvent.SetEvent;
  FTextSearcher.Start;
end;

procedure TSearchForm.Button2Click(Sender: TObject);
begin
  FTextSearcher.Init(Edit1.Text, Memo1.Lines, FFindIndex + 2);
  FEvent.SetEvent;
  FTextSearcher.Start;
end;

procedure TSearchForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FTextSearcher.Terminate;
  CloseAction := caFree;
end;

constructor TSearchForm.Create(anOwner: TComponent);
begin
  inherited Create(anOwner);
  FEvent := TEvent.Create(nil,False, False, 'SearchThreadEvent'+IntToStr(FormCount));
  FTextSearcher := TTextSearchThread.Create(True, FEvent);
  FTextSearcher.OnShowStatus:= @OnFindText;
  FFindIndex := 1;
end;

destructor TSearchForm.Destroy;
begin
  FTextSearcher.Terminate;
  FEvent.ResetEvent;
  FEvent.Free;
  inherited Destroy;
end;

procedure TSearchForm.OnFindText(FindIndex: Integer; Length: Integer; SearchStatus: TSearchStatus);
begin
  case  SearchStatus of
     ssFind: begin
       Memo1.SetFocus;
       Memo1.SelStart := FindIndex;
       Memo1.SelLength := Length;
       FFindIndex := FindIndex;
     end;
     ssNotFind: begin
       ListBox1.AddItem('Не найдено', TObject(1));
     end;
     ssTimout: begin
       ListBox1.AddItem('Превышение времени', TObject(1));
     end;
  end
end;

end.

