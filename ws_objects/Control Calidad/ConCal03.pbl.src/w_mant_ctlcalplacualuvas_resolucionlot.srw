$PBExportHeader$w_mant_ctlcalplacualuvas_resolucionlot.srw
$PBExportComments$Ventana de Resolución de Planilla Cualitativa
forward
global type w_mant_ctlcalplacualuvas_resolucionlot from w_mant_detalle
end type
end forward

global type w_mant_ctlcalplacualuvas_resolucionlot from w_mant_detalle
integer x = 123
integer y = 96
integer width = 1851
integer height = 864
string title = "Resolución"
end type
global w_mant_ctlcalplacualuvas_resolucionlot w_mant_ctlcalplacualuvas_resolucionlot

type variables
uo_ctlcaldanoespecie		iuo_ctlcaldanoespecie
end variables

forward prototypes
public subroutine wf_nuevo ()
end prototypes

public subroutine wf_nuevo ();dw_1.SetRedraw(False)
dw_1.SetRow(il_fila)
dw_1.ScrollToRow(il_fila)
dw_1.SetRedraw(True)
end subroutine

on w_mant_ctlcalplacualuvas_resolucionlot.create
call super::create
end on

on w_mant_ctlcalplacualuvas_resolucionlot.destroy
call super::destroy
end on

event ue_recuperadatos();w_main.SetMicroHelp("Recuperando Datos...")

SetPointer(HourGlass!)
PostEvent("ue_listo")

IF istr_mant.Agrega THEN
	pb_Cancela.Enabled	=	False
	pb_Primero.Enabled	=	False
	pb_Anterior.Enabled	=	False
	pb_Siguiente.Enabled	=	False
	pb_Ultimo.Enabled		=	False
	
	wf_nuevo()
ELSE
	IF dw_1.RowCount() > 1 THEN
		pb_Primero.Enabled	=	True
		pb_Anterior.Enabled	=	True
		pb_Siguiente.Enabled	=	True
		pb_Ultimo.Enabled		=	True
	END IF
	
	il_fila	=	istr_mant.dw2.GetRow()
	
	dw_1.SetRedraw(False)
	dw_1.SetRow(il_fila)
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRedraw(True)

	IF istr_mant.Borra THEN
		dw_1.Enabled		=	False
		pb_Salir.Enabled	=	False
	ELSEIF istr_mant.Solo_Consulta THEN
		dw_1.Enabled			=	False
		pb_Acepta.Enabled		=	False
		pb_Cancela.Enabled	=	False
	ELSE
		pb_Salir.Enabled	=	False
	END IF
END IF

ias_Campo[1]  = dw_1.Object.ccce_resolu[il_Fila]
ias_Campo[2]  = String(dw_1.Object.ccce_causal[il_Fila])

IF NOT IsNull(dw_1.Object.ccce_resolu[il_Fila]) THEN
	IF dw_1.Object.ccce_resolu[il_Fila] = "O" THEN
		dw_1.Object.ccce_causal.Protect	=	0
		dw_1.Object.ccce_causal.BackGround.Color	=	RGB(255,255,255)
	ELSE
		dw_1.Object.ccce_causal.Protect	=	1
		dw_1.Object.ccce_causal.BackGround.Color	=	RGB(192,192,192)
	END IF
END IF
end event

event ue_deshace();call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_Fila, "ccce_resolu", ias_Campo[1])
	dw_1.SetItem(il_Fila, "ccce_causal", Integer(ias_Campo[2]))
END IF
end event

event ue_antesguardar();String	ls_Mensaje, ls_Columna[]
Integer	li_Contador

IF Isnull(dw_1.Object.ccce_resolu[il_Fila]) OR dw_1.Object.ccce_resolu[il_Fila] = "" THEN
	li_Contador	++
	ls_Mensaje 			= ls_Mensaje + "~nResolución Lote"
	ls_Columna[li_Contador]	= "ccce_resolu"
END IF

IF Isnull(dw_1.Object.ccce_resolu[1]) = False AND dw_1.Object.ccce_resolu[1] = "O" THEN
	IF Isnull(dw_1.Object.ccce_causal[il_Fila]) OR dw_1.Object.ccce_causal[il_Fila] = 0 THEN
		li_Contador	++
		ls_Mensaje 			= ls_Mensaje + "~nCausal"
		ls_Columna[li_Contador]	= "ccce_causal"
	END IF
END IF

IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event open;x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")
istr_Mant = Message.PowerObjectParm
istr_Mant.dw2.ShareData(dw_1)

dw_1.SetTransObject(SqlCa)

iuo_ctlcaldanoespecie	=	Create uo_ctlcaldanoespecie

dw_1.Object.ccce_causal.Protect	=	1
dw_1.Object.ccce_causal.BackGround.Color	=	RGB(192,192,192)
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_ctlcalplacualuvas_resolucionlot
boolean visible = false
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_ctlcalplacualuvas_resolucionlot
boolean visible = false
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_ctlcalplacualuvas_resolucionlot
boolean visible = false
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_ctlcalplacualuvas_resolucionlot
boolean visible = false
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_ctlcalplacualuvas_resolucionlot
integer x = 1554
integer y = 300
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_ctlcalplacualuvas_resolucionlot
integer x = 1554
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_ctlcalplacualuvas_resolucionlot
integer x = 1554
integer y = 476
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_ctlcalplacualuvas_resolucionlot
integer y = 76
integer width = 1339
integer height = 560
string dataobject = "dw_mant_ctlcalplacualuvas_resolucionlot"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_Null

SetNull(ls_Null)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna

	CASE "ccce_resolu"
		IF Data	=	"O" THEN
			This.Object.ccce_causal.Protect	=	0
			This.Object.ccce_causal.BackGround.Color	=	RGB(255,255,255)
			This.SetColumn("ccce_causal")
		ELSE
			This.Object.ccce_causal.Protect	=	1
			This.Object.ccce_causal.BackGround.Color	=	RGB(192,192,192)
			This.SetItem(Row, "ccce_causal", Integer(ls_Null))
		END IF

	CASE "ccce_causal"
		IF NOT iuo_ctlcaldanoespecie.Existe(Integer(istr_Mant.Argumento[7]), &
														Integer(Data), True, SqlCa) THEN
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF

END CHOOSE
end event

