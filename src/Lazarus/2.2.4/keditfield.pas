// -------------------------------------------------------------------------
// File: kEditField.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
{$mode delphi}{$H+}
unit kEditField;

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TkEditField = class(TEdit)
  private
  protected
  public
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('KALLUP',[TkEditField]);
end;

initialization
{$I kComponents.lrs}

end.
