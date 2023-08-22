$PBExportHeader$w_mant_deta_pronvariecalibre.srw
forward
global type w_mant_deta_pronvariecalibre from w_mant_detalle
end type
type dw_2 from uo_dw within w_mant_deta_pronvariecalibre
end type
type pb_insertar from picturebutton within w_mant_deta_pronvariecalibre
end type
type pb_elimina from picturebutton within w_mant_deta_pronvariecalibre
end type
end forward

global type w_mant_deta_pronvariecalibre from w_mant_detalle
integer width = 2295
integer height = 1844
string title = "CONDICION"
dw_2 dw_2
pb_insertar pb_insertar
pb_elimina pb_elimina
end type
global w_mant_deta_pronvariecalibre w_mant_deta_pronvariecalibre

type variables
DataWindowChild   	dw_variedades
Long						il_filadw2
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public function boolean duplicadodw2 (string as_calibre)
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String	ls_Variedad, ls_Calibre

ls_Variedad	=	String(dw_1.Object.vari_codigo[il_Fila])
ls_Calibre	=	String(dw_1.Object.vaca_calibr[il_Fila])

CHOOSE CASE as_Columna
	CASE "vari_codigo"
		ls_Variedad		=	as_Valor
	
	CASE "vaca_calibr"
		ls_Calibre		=	as_Valor
	
END CHOOSE

ll_fila	= istr_mant.dw.Find("vari_codigo = " + ls_variedad + " AND " + &
							"Trim(vaca_calibr) = '" + Trim(ls_Calibre) + "'", &
							1 , istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_FilaAnc THEN 
	MessageBox("Atención","Registro ya fue ingresado anteriormente.",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean duplicadodw2 (string as_calibre);Long		ll_fila

ll_fila	= dw_2.Find("Trim(vaca_calhom) = '" + Trim(as_calibre) + "'",1, dw_2.RowCount())

IF ll_fila > 0 and ll_fila <> il_filadw2 THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
END IF

RETURN False
end function

on w_mant_deta_pronvariecalibre.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.pb_insertar=create pb_insertar
this.pb_elimina=create pb_elimina
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.pb_insertar
this.Control[iCurrent+3]=this.pb_elimina
end on

on w_mant_deta_pronvariecalibre.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.pb_insertar)
destroy(this.pb_elimina)
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.GetItemNumber(il_fila, "espe_codigo"))  
ias_campo[2] = String(dw_1.GetItemNumber(il_fila, "vari_codigo")) 
ias_campo[3] = dw_1.GetItemString(il_fila, "vaca_calibr")
ias_campo[4] = String(dw_1.GetItemDecimal(il_fila, "vaca_milini"))  
ias_campo[5] = String(dw_1.GetItemDecimal(il_fila, "vaca_milfin"))  
ias_campo[6] = String(dw_1.GetItemNumber(il_fila, "vaca_grucal")) 

dw_2.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), dw_1.Object.vaca_calibr[il_Fila])
il_filadw2	=	dw_2.GetRow()

If Not istr_Mant.Agrega And Not istr_Mant.Borra Then
	dw_1.Object.vaca_calibr.Protect = 1
	dw_1.Object.vaca_calibr.BackGround.Color = 553648127
	dw_1.Object.vaca_calibr.Color = RGB(255,255,255)
Else
	dw_1.Object.espe_codigo[il_fila] = Integer(istr_mant.Argumento[1])
	dw_1.Object.vari_codigo[il_fila] = Integer(istr_mant.Argumento[2])
End If

end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "vaca_calibr", ias_campo[3])
	dw_1.SetItem(il_fila, "vaca_milini", Dec(ias_campo[4]))
	dw_1.SetItem(il_fila, "vaca_milfin", Dec(ias_campo[5]))
	dw_1.SetItem(il_fila, "vaca_grucal", Dec(ias_campo[6]))
End If
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_calibr, ls_colu[]

IF Isnull(dw_1.GetItemString(il_fila, "vaca_calibr")) THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCalibre"
	ls_colu[li_cont]	= "vaca_calibr"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.espe_codigo[il_fila] = Integer(istr_mant.Argumento[1])
dw_1.Object.vari_codigo[il_fila] = Integer(istr_mant.Argumento[2])
end event

event open;call super::open;dw_1.GetChild("vari_codigo", dw_variedades)
dw_variedades.SetTransObject(SQLCA)
dw_variedades.Retrieve(Integer(istr_mant.argumento[1]))

dw_2.SetTransObject(sqlca)
istr_mant.dw2.ShareData(dw_2)


end event

event resize;Integer		li_posic_x, li_posic_y, li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

This.Height			=	dw_1.Height + dw_2.Height + 400
This.Width			=	dw_1.width + 600

dw_1.x				=	78
dw_1.y				=	100	

li_posic_x			=	This.WorkSpaceWidth() - 400
li_posic_y			=	dw_1.y

pb_acepta.x			=	li_posic_x
pb_acepta.y			=	li_posic_y
pb_acepta.width	=	li_Ancho
pb_acepta.height	=	li_Alto
li_posic_y+=li_Siguiente

pb_cancela.x		=	li_posic_x
pb_cancela.y		=	li_posic_y
pb_cancela.width	=	li_Ancho
pb_cancela.height	=	li_Alto
li_posic_y+=li_Siguiente

pb_salir.x			=	li_posic_x
pb_salir.y			=	li_posic_y
pb_salir.width		=	li_Ancho
pb_salir.height		=	li_Alto

li_posic_y			=	dw_2.y + dw_2.height - li_Siguiente

pb_elimina.x			=	li_posic_x
pb_elimina.y			=	li_posic_y
pb_elimina.width		=	li_Ancho
pb_elimina.height		=	li_Alto
li_posic_y -=li_Siguiente

pb_insertar.x			=	li_posic_x
pb_insertar.y			=	li_posic_y
pb_insertar.width		=	li_Ancho
pb_insertar.height		=	li_Alto
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_pronvariecalibre
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_pronvariecalibre
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_pronvariecalibre
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_pronvariecalibre
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_pronvariecalibre
integer x = 1906
integer y = 452
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_pronvariecalibre
integer x = 1906
integer y = 236
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_pronvariecalibre
integer x = 1902
integer y = 668
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_pronvariecalibre
integer y = 104
integer width = 1705
integer height = 852
string dataobject = "dw_mant_pronvariecalibre"
end type

event dw_1::itemchanged;call super::itemchanged;String		ls_columna, ls_null, ls_Calibre
Integer	li_variedad, li_Especie

ls_columna 	=	GetColumnName()
SetNull(ls_null)


Choose Case ls_columna		
	Case "vaca_calibr"				
		If Duplicado(ls_Columna, data) Then
			dw_1.SetItem(il_fila, ls_columna, ls_null)
			Return 1
		End If
		
End Choose
end event

type dw_2 from uo_dw within w_mant_deta_pronvariecalibre
integer x = 453
integer y = 976
integer width = 891
integer height = 700
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string title = "Calibres Homologados"
string dataobject = "dw_mant_mues_calibresdeta"
end type

event itemchanged;call super::itemchanged;String		ls_Columna, ls_Null

ls_Columna = dwo.Name
SetNull(ls_Null)

CHOOSE CASE ls_Columna
	CASE "vaca_calhom"		
		IF Duplicadodw2(data) THEN
			This.SetItem(Row, ls_columna, ls_Null)
			RETURN 1
		END IF
		
END CHOOSE
end event

event itemerror;call super::itemerror;Return 1
end event

type pb_insertar from picturebutton within w_mant_deta_pronvariecalibre
integer x = 1765
integer y = 1176
integer width = 302
integer height = 244
integer taborder = 21
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Mas.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Mas-bn.png"
alignment htextalign = left!
end type

event clicked;il_filadw2 = dw_2.InsertRow(0)

dw_2.Object.espe_codigo[il_filadw2] = dw_1.Object.espe_codigo[il_fila]
dw_2.Object.vari_codigo[il_filadw2] = dw_1.Object.vari_codigo[il_fila]
dw_2.Object.vaca_calibr[il_filadw2] = dw_1.Object.vaca_calibr[il_fila]

dw_2.ScrollToRow(il_filadw2)
dw_2.SetRow(il_filadw2)
dw_2.SetFocus()
dw_2.SetColumn(1)
end event

type pb_elimina from picturebutton within w_mant_deta_pronvariecalibre
integer x = 1765
integer y = 1424
integer width = 302
integer height = 244
integer taborder = 31
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Menos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Menos-bn.png"
alignment htextalign = left!
end type

event clicked;IF dw_2.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0
This.TriggerEvent ("ue_validaborrar")
IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox("","No se puede borrar actual registro.")
	END IF

	IF dw_2.RowCount() = 0 THEN
		il_filadw2 = 0
	ELSE
		il_filadw2 = dw_2.GetRow()
	END IF
END IF
end event

