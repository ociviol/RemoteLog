unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SynEdit;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    SynEdit1: TSynEdit;
  private

  public

  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

end.

