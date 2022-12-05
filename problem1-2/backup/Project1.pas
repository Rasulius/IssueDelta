program Project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TRandomDigitsApplication }

  TRandomDigits =  array of Integer;
  TRandomDigitsApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    { генерировать случайные числа}
    function GenerateRandomDigits(var FindIndex: Integer; const Size: Integer = 200000; const Offset: Integer = 500000): TRandomDigits;
    { распечатать все числа}
    procedure PrintAllDigits(Digits: TRandomDigits);
     { напечатать окрестность числа}
     procedure PrintNumberNeighborhood(FindIndex: Integer; Digits: TRandomDigits);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TRandomDigits }

procedure TRandomDigitsApplication.DoRun;
var
  ErrorMsg: String;
  RandomDigits: TRandomDigits;
  FindDigitIndex: Integer;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  RandomDigits := GenerateRandomDigits(FindDigitIndex, 200000, 20001);


  //PrintAllDigits(RandomDigits);
  {проверка распечаем окрестность числа, чтобы проверить что 20000 там есть}
  PrintNumberNeighborhood(FindDigitIndex, RandomDigits);
  readln;
  // stop program loop
  Terminate;
end;

function TRandomDigitsApplication.GenerateRandomDigits(var FindIndex: Integer;
  const Size: Integer = 200000;  const Offset: Integer = 500000): TRandomDigits;
const
  SearchDigit = 20000;
var
   i: Integer;
   IsNumberFound: Boolean;
   RandomDidits: TRandomDigits;
begin
   Randomize;

   SetLength(RandomDidits, Size);
   FindIndex := -1;

   repeat
     IsNumberFound := False;

     for i:= 0 to High(RandomDidits) do begin
       RandomDidits[i] := Random(Offset) + 1;
       if RandomDidits[i] = SearchDigit then begin
         IsNumberFound := True;
         FindIndex := i;
       end;
     end;
   until (IsNumberFound);

   Result := RandomDidits;
end;

procedure TRandomDigitsApplication.PrintAllDigits(Digits: TRandomDigits);
var
  i: Integer;
begin
  for i:= 0 to High(Digits) do begin
    Write(Digits[i], ',')
  end;
end;

procedure TRandomDigitsApplication.PrintNumberNeighborhood(FindIndex: Integer;
  Digits: TRandomDigits);
var
  i: Integer;
begin
 {Число найдено в начале массива распечатаем первые 10}
  if ((FindIndex <= 10) and (high(Digits)>=10)) then begin
    for i:= 0 to 10 do begin
       Writeln('Digit[', i,'] ', Digits[i]);
    end;
  {Найденное число в середине, напечатаем окрестность числа}
  end else if((FindIndex>10) and ((FindIndex+5)< high(Digits))) then begin
    for i:= FindIndex - 5 to FindIndex + 5 do begin
       Writeln('Digit[', i,'] ', Digits[i]);
    end;
  { найденное число в конце, распечатаем последние 10 }
  end else begin
    for i:= high(Digits) - 10 to high(Digits) do begin
       Writeln('Digit[', i,'] ', Digits[i]);
    end;
  end;
end;

constructor TRandomDigitsApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TRandomDigitsApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TRandomDigitsApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TRandomDigitsApplication;
begin
  Application:=TRandomDigitsApplication.Create(nil);
  Application.Title:='Random digits';
  Application.Run;
  Application.Free;
end.

