$PBExportHeader$w_mant_deta_ordenproceso_comercial.srw
$PBExportComments$Mantención Detalle de Ordenes de Proceso
forward
global type w_mant_deta_ordenproceso_comercial from w_mant_detalle_csd
end type
type dw_2 from uo_dw within w_mant_deta_ordenproceso_comercial
end type
type pb_ins_det from picturebutton within w_mant_deta_ordenproceso_comercial
end type
type pb_eli_det from picturebutton within w_mant_deta_ordenproceso_comercial
end type
type st_1 from statictext within w_mant_deta_ordenproceso_comercial
end type
type em_total from editmask within w_mant_deta_ordenproceso_comercial
end type
type dw_3 from uo_dw within w_mant_deta_ordenproceso_comercial
end type
type st_3 from statictext within w_mant_deta_ordenproceso_comercial
end type
type cb_aplicar from commandbutton within w_mant_deta_ordenproceso_comercial
end type
type cb_aplicatodo from commandbutton within w_mant_deta_ordenproceso_comercial
end type
type st_4 from statictext within w_mant_deta_ordenproceso_comercial
end type
type cb_desaplica from commandbutton within w_mant_deta_ordenproceso_comercial
end type
type cb_desaplicatodo from commandbutton within w_mant_deta_ordenproceso_comercial
end type
type st_5 from statictext within w_mant_deta_ordenproceso_comercial
end type
end forward

global type w_mant_deta_ordenproceso_comercial from w_mant_detalle_csd
integer width = 3442
integer height = 1848
string title = "DETALLE ORDEN DE PROCESO"
event ue_nuevo_detalle ( )
event ue_borra_detalle ( )
dw_2 dw_2
pb_ins_det pb_ins_det
pb_eli_det pb_eli_det
st_1 st_1
em_total em_total
dw_3 dw_3
st_3 st_3
cb_aplicar cb_aplicar
cb_aplicatodo cb_aplicatodo
st_4 st_4
cb_desaplica cb_desaplica
cb_desaplicatodo cb_desaplicatodo
st_5 st_5
end type
global w_mant_deta_ordenproceso_comercial w_mant_deta_ordenproceso_comercial

type variables
DataWindowChild	idwc_especie, idwc_calibre, idwc_lineapacking, idwc_camara, idwc_envase, &
						idwc_variedad, idwc_calidad
uo_ProdCuarteles		iuo_Cuartel
uo_Productores		iuo_Productor

String	is_rutprod
Integer	il_Fila_det
Boolean  lb_modifica=False
Long     il_total

str_envase			istr_Envase
end variables

forward prototypes
public subroutine captura_totalbultos ()
end prototypes

event ue_borra_detalle();Integer	li_Fila

IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_2.RowCount() = 0 THEN
		pb_eli_det.Enabled = False
	ELSE
		il_fila = dw_2.GetRow()
		il_fila_det = dw_2.GetRow()
	END IF
END IF
end event

public subroutine captura_totalbultos ();Long	ll_Fila,ll_Total_Bultos, ll_bultos_ant

ll_Fila	=	dw_2.RowCount()

IF ll_Fila > 0 THEN
	ll_Total_Bultos		=	dw_2.Object.total_bultos[ll_Fila]
END IF

ll_bultos_ant = dw_1.Object.orpr_canbul[il_Fila]
IF isnull(ll_bultos_ant) THEN ll_bultos_ant=0
	dw_1.Object.saldo[il_fila]       = ll_bultos_ant + dw_1.Object.saldo[il_fila] - ll_Total_Bultos
	dw_1.Object.orpr_canbul[il_Fila]	= ll_Total_Bultos
RETURN
end subroutine

on w_mant_deta_ordenproceso_comercial.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.pb_ins_det=create pb_ins_det
this.pb_eli_det=create pb_eli_det
this.st_1=create st_1
this.em_total=create em_total
this.dw_3=create dw_3
this.st_3=create st_3
this.cb_aplicar=create cb_aplicar
this.cb_aplicatodo=create cb_aplicatodo
this.st_4=create st_4
this.cb_desaplica=create cb_desaplica
this.cb_desaplicatodo=create cb_desaplicatodo
this.st_5=create st_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.pb_ins_det
this.Control[iCurrent+3]=this.pb_eli_det
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.em_total
this.Control[iCurrent+6]=this.dw_3
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.cb_aplicar
this.Control[iCurrent+9]=this.cb_aplicatodo
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.cb_desaplica
this.Control[iCurrent+12]=this.cb_desaplicatodo
this.Control[iCurrent+13]=this.st_5
end on

on w_mant_deta_ordenproceso_comercial.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.pb_ins_det)
destroy(this.pb_eli_det)
destroy(this.st_1)
destroy(this.em_total)
destroy(this.dw_3)
destroy(this.st_3)
destroy(this.cb_aplicar)
destroy(this.cb_aplicatodo)
destroy(this.st_4)
destroy(this.cb_desaplica)
destroy(this.cb_desaplicatodo)
destroy(this.st_5)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Integer	li_Fila, li_FilaClasif, li_CtaFila, li_seguimiento, li_null
string   ls_null
Long ll_prod_cod
setnull(ls_null)

dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])

ll_prod_cod = Long(istr_Mant.Argumento[6])
li_Fila	=	dw_3.Retrieve(Integer(istr_Mant.Argumento[2]), &
								  Integer(istr_Mant.Argumento[5]), &
								  ll_prod_cod, &
								  Integer(istr_Mant.Argumento[7]), &
								  istr_Mant.Argumento[8],Integer(istr_Mant.Argumento[13]))
	
IF li_Fila > 0 THEN
	cb_aplicar.Enabled		=	True
	cb_aplicatodo.Enabled	=	True
END IF

dw_2.SetFilter("")
dw_2.Filter()
dw_2.SetFilter("orpr_numero = " + istr_mant.Argumento[9])
dw_2.Filter()

li_seguimiento = dw_1.Object.orpr_niveld[1]

//IF li_seguimiento = 2 THEN
//   dw_3.SetFilter("")
//   dw_3.Filter()
//	dw_3.SetFilter("if( isnull( prbr_codpre ),1,0)  = 0")
//   dw_3.Filter()
//END IF
//
//IF li_seguimiento = 3 THEN
//   dw_3.SetFilter("")
//   dw_3.Filter()
//	dw_3.SetFilter("if( isnull(prbr_codpre) and isnull(prcc_codigo),1,0)  = 0")
//   dw_3.Filter()
//END IF
//	
//IF li_seguimiento = 1 THEN
//   dw_3.SetFilter("")
//   dw_3.Filter()
//END IF


li_CtaFila	=	dw_2.RowCount()

FOR li_Fila	=	1 TO li_CtaFila
	li_FilaClasif	=	dw_3.Find("lofc_pltcod	 	= " + String(dw_2.Object.lote_pltcod[li_Fila]) 	+ " AND " + &
										 "lofc_espcod 	= " + String(dw_2.Object.lote_espcod[li_Fila]) 	+ " AND " + &
										 "lofc_lotefc 		= " + String(dw_2.Object.lote_codigo[li_Fila]) 	+ " AND " + &
										 "enva_tipoen 	= " + String(dw_2.Object.enva_tipoen[li_Fila])	+ " AND " + &
										 "enva_codigo 	= " + String(dw_2.Object.enva_codigo[li_Fila])	+ " AND " + &
										 "cama_codigo 	= " + String(dw_2.Object.cama_codigo[li_Fila]), &
										 1, dw_3.RowCount())
	
	IF li_FilaClasif > 0 THEN
		dw_3.DeleteRow(li_FilaClasif)
	END IF
NEXT

IF dw_3.RowCount() > 0 THEN
	cb_aplicar.Enabled			=	True
	cb_aplicatodo.Enabled		=	True
END IF

IF dw_2.RowCount() > 0 THEN
	cb_desaplica.Enabled			=	True
	cb_desaplicatodo.Enabled	=	True
END IF

Captura_TotalBultos()
end event

event ue_deshace();call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
END IF
end event

event open;/* 
Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Especie
istr_Mant.Argumento[3]	=	Fecha Programa
istr_Mant.Argumento[4]	=	Fecha Proceso
istr_Mant.Argumento[5]	=	Variedad
istr_Mant.Argumento[6]	=	Productor
istr_Mant.Argumento[7]	=	Periodo Frio
istr_Mant.Argumento[8]	=	Tratamiento Frio
istr_Mant.Argumento[9]	=	Numero de Orden
istr_Mant.Argumento[13]	=	Cliente
*/

Integer	li_Cliente

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

li_Cliente	=	Integer(istr_mant.argumento[13])

dw_1.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve(li_Cliente)

dw_1.GetChild("vari_codigo",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
idwc_variedad.Retrieve(Integer(istr_Mant.Argumento[2]))

dw_1.Getchild("line_codigo", idwc_lineapacking)
idwc_lineapacking.SetTransObject(sqlca)
idwc_lineapacking.Retrieve(Integer(istr_Mant.Argumento[1]))

dw_2.Getchild("enva_codigo", idwc_envase)
idwc_envase.SetTransObject(sqlca)
idwc_envase.Retrieve(0)

dw_2.Getchild("cama_codigo", idwc_camara)
idwc_camara.SetTransObject(sqlca)
idwc_camara.Retrieve(Integer(istr_Mant.Argumento[1]))

dw_2.Getchild("cate_codigo", idwc_calidad)
idwc_calidad.SetTransObject(sqlca)
idwc_calidad.Retrieve()

dw_3.Getchild("enva_codigo", idwc_envase)
idwc_envase.SetTransObject(sqlca)
idwc_envase.Retrieve(0)

dw_3.Getchild("cama_codigo", idwc_camara)
idwc_camara.SetTransObject(sqlca)
idwc_camara.Retrieve(Integer(istr_Mant.Argumento[1]))

dw_3.Getchild("cate_codigo", idwc_calidad)
idwc_calidad.SetTransObject(sqlca)
idwc_calidad.Retrieve()

dw_1.SetTransObject(Sqlca)
istr_mant.dw.ShareData(dw_1)

dw_2.Modify("datawindow.message.title='Error '+ is_titulo")
dw_2.SetRowFocusIndicator(Hand!)
dw_2.Modify("DataWindow.Footer.Height = 84")

dw_3.SetRowFocusIndicator(Hand!)
dw_3.Modify("DataWindow.Footer.Height = 84")

dw_2.SetTransObject(Sqlca)
istr_mant.dw2.ShareData(dw_2)

dw_3.SetTransObject(Sqlca)

end event

event closequery;IF Not istr_mant.Borra THEN

	IF istr_mant.Agrega AND istr_mant.Respuesta <> 1 THEN 
		dw_1.DeleteRow(il_fila)
		dw_2.RowsMove(1,dw_2.RowCount(),Primary!,dw_2,1,Delete!)
		
		IF dw_1.RowCount() > 0 THEN
			dw_1.SelectRow(0, False)
			dw_1.SelectRow(dw_1.RowCount(), True)
		END IF
		
		RETURN
	END IF

	IF ib_Modifica AND istr_mant.Respuesta = 1 THEN
		This.TriggerEvent("ue_guardar")
		
		IF Message.DoubleParm = -1 THEN Message.ReturnValue = 1
		
		RETURN
	ELSEIF istr_mant.Respuesta = 2 THEN
		This.TriggerEvent("ue_deshace")
	END IF
END IF
end event

event ue_nuevo();call super::ue_nuevo;dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])

end event

event ue_borrar();call super::ue_borrar;Long	ll_fila1

if dw_2.rowcount() < 1 then return
if messagebox("Borrar registro(s)","Desea Eliminar la fila seleccionada ?",&
				exclamation!,yesno!,2) <> 1 then
	return
end if
setpointer(hourglass!)
ib_borrar = true
w_main.setmicrohelp("Validando la eliminación...")
message.doubleparm = 0
this.triggerevent ("ue_validaborrar")
if message.doubleparm = -1 then return

ll_fila1=dw_2.GetRow()

IF dw_2.RowsCopy(ll_fila1,ll_fila1,Primary!,dw_3,dw_3.RowCount()+1,Primary!) > 0 THEN
	dw_2.DeleteRow(ll_fila1)
	w_main.setmicrohelp("Registro Borrado.")
	lb_modifica = TRUE
	setpointer(Arrow!)
else
	ib_borrar = false
	messagebox(this.title,"No se puede borrar registro actual.")
end if

end event

event ue_antesguardar();call super::ue_antesguardar;Integer	li_Secuencia, li_Fila

li_Secuencia	=	1

FOR li_Fila = 1 TO dw_2.RowCount()
	dw_2.Object.orpr_numero[li_Fila]	=	dw_1.Object.orpr_numero[il_Fila]
	dw_2.Object.orpd_secuen[li_Fila]	=	li_Secuencia
   IF lb_modifica=FALSE THEN dw_2.Setitemstatus(li_fila,0,Primary!,Notmodified!)
	li_Secuencia ++
NEXT

IF lb_modifica=FALSE THEN dw_1.Setitemstatus(il_fila,0,Primary!,Notmodified!)

dw_2.SetFilter("")
dw_2.Filter()
end event

event ue_guardar();SetPointer(HourGlass!)

Message.DoubleParm = 0

w_main.SetMicroHelp("Grabando información...")
TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN
end event

event resize;//
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_ordenproceso_comercial
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_ordenproceso_comercial
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_ordenproceso_comercial
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_ordenproceso_comercial
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_ordenproceso_comercial
integer x = 3159
integer y = 284
integer taborder = 50
boolean enabled = false
end type

event pb_cancela::clicked;istr_mant.respuesta = 2

dw_2.SetFilter("")
dw_2.Filter()

CloseWithReturn(Parent, istr_mant)
end event

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_ordenproceso_comercial
integer x = 3159
integer y = 108
integer taborder = 40
boolean default = false
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE
	Parent.TriggerEvent("ue_antesguardar")
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_ordenproceso_comercial
integer x = 3159
integer y = 456
integer taborder = 60
end type

event pb_salir::clicked;IF istr_mant.Agrega THEN 
	//Descuenta último Lote generado
	istr_mant.argumento[7]	=	String(Integer(istr_mant.argumento[7])-1)
END IF

CALL SUPER::Clicked
end event

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_ordenproceso_comercial
integer y = 68
integer width = 2903
integer height = 408
string dataobject = "dw_mues_ordenproceso"
end type

type dw_2 from uo_dw within w_mant_deta_ordenproceso_comercial
integer x = 453
integer y = 1124
integer width = 2583
integer height = 600
integer taborder = 20
boolean bringtotop = true
boolean titlebar = true
string title = "LOTES SELECCIONADOS"
string dataobject = "dw_mues_spro_ordenprocdeta_comercial"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF row > 0 THEN
	IF IsSelected(row) THEN
		SelectRow(row,False)
	ELSE
		SelectRow(row,True)
	END IF
END IF
end event

event itemerror;call super::itemerror;RETURN 1
end event

event losefocus;call super::losefocus;Accepttext()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila_det = CurrentRow
END IF
end event

type pb_ins_det from picturebutton within w_mant_deta_ordenproceso_comercial
boolean visible = false
integer x = 3159
integer y = 1352
integer width = 155
integer height = 132
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\inserte.bmp"
string disabledname = "\desarrollo\bmp\insertd.bmp"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_nuevo_detalle")
end event

type pb_eli_det from picturebutton within w_mant_deta_ordenproceso_comercial
boolean visible = false
integer x = 3159
integer y = 1532
integer width = 155
integer height = 132
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\suprime.bmp"
string disabledname = "\desarrollo\bmp\suprimd.bmp"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_borra_detalle")
end event

type st_1 from statictext within w_mant_deta_ordenproceso_comercial
boolean visible = false
integer x = 599
integer y = 1004
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Total Cajas"
boolean focusrectangle = false
end type

type em_total from editmask within w_mant_deta_ordenproceso_comercial
boolean visible = false
integer x = 978
integer y = 992
integer width = 402
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#######"
end type

event modified;dw_2.Enabled 			= TRUE
pb_ins_det.Enabled	= TRUE
pb_eli_det.Enabled	= TRUE

il_total= Long(em_total.text)
end event

type dw_3 from uo_dw within w_mant_deta_ordenproceso_comercial
integer x = 448
integer y = 520
integer width = 2583
integer height = 600
integer taborder = 30
boolean bringtotop = true
boolean titlebar = true
string title = "LOTES CLASIFICADOS"
string dataobject = "dw_mant_spro_lotes_clasificados_comercia"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF row > 0 THEN
	IF IsSelected(row) THEN
		SelectRow(row,False)
	ELSE
		SelectRow(row,True)
	END IF
END IF
end event

event itemerror;call super::itemerror;RETURN 1
end event

event losefocus;call super::losefocus;Accepttext()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila_det = CurrentRow
END IF
end event

type st_3 from statictext within w_mant_deta_ordenproceso_comercial
integer x = 32
integer y = 520
integer width = 416
integer height = 600
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_aplicar from commandbutton within w_mant_deta_ordenproceso_comercial
integer x = 64
integer y = 688
integer width = 352
integer height = 104
integer taborder = 120
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Aplica"
end type

event clicked;Long		ll_fila1, ll_fila2
Integer	li_causal

ll_fila2 = dw_3.GetSelectedRow(0)

IF ll_fila2 = 0 THEN
	MessageBox("Error de Consistencia","Debe seleccionar Lotes Clasificados previamente ")
	RETURN
END IF

SetPointer(HourGlass!)

DO WHILE ll_fila2 > 0
	ll_fila1	=	dw_2.RowCount() + 1

	dw_3.RowsMove(ll_fila2,ll_fila2,Primary!,dw_2,ll_fila1,Primary!)

	cb_desaplica.Enabled	= True
	cb_desaplicatodo.Enabled	= True

	ll_fila2 = dw_3.GetSelectedRow(0)
LOOP

lb_modifica = TRUE
Captura_TotalBultos()

dw_2.SetFocus()


end event

type cb_aplicatodo from commandbutton within w_mant_deta_ordenproceso_comercial
integer x = 64
integer y = 848
integer width = 352
integer height = 104
integer taborder = 130
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Aplica Todos"
end type

event clicked;Long		ll_fila2,ll_fila1
Boolean	lb_respuesta = True

ll_fila2 = dw_3.Rowcount()

IF ll_fila2 = 0 THEN
	MessageBox("Error de Consistencia","No Existe Lotes Clasificados")
	RETURN 
END IF

SetPointer(HourGlass!)

IF lb_respuesta THEN
	ll_fila1	=	dw_2.RowCount() + 1
	dw_3.RowsMove(1,ll_fila2,Primary!,dw_2,ll_fila1,Primary!)
	
	cb_desaplica.Enabled	= True
	cb_desaplicatodo.Enabled	= True

	dw_2.SetFocus()
ELSE
	dw_3.SelectRow(ll_fila2,False)
	dw_3.SetFocus()
END IF
lb_modifica = TRUE
Captura_TotalBultos()

end event

type st_4 from statictext within w_mant_deta_ordenproceso_comercial
integer x = 32
integer y = 1120
integer width = 416
integer height = 600
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_desaplica from commandbutton within w_mant_deta_ordenproceso_comercial
integer x = 64
integer y = 1276
integer width = 352
integer height = 104
integer taborder = 140
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Desaplica "
end type

event clicked;Parent.TriggerEvent("ue_borrar")

Captura_TotalBultos()
end event

type cb_desaplicatodo from commandbutton within w_mant_deta_ordenproceso_comercial
integer x = 64
integer y = 1436
integer width = 352
integer height = 104
integer taborder = 150
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Desapl. Todo"
end type

event clicked;Long		ll_filas

if dw_2.rowcount() < 1 then return
if messagebox("Borrar registro(s)","Desea Eliminar TODAS LAS FILAS asignadas ?",&
				exclamation!,yesno!,2) <> 1 then
	return
end if

SetPointer(hourglass!)

ib_borrar = true
w_main.setmicrohelp("Validando la eliminación...")
message.doubleparm = 0
Parent.triggerevent ("ue_validaborrar")
if message.doubleparm = -1 then return
dw_2.RowsCopy(1,dw_2.RowCount(),Primary!,dw_3,dw_3.RowCount()+1,Primary!)
dw_2.RowsMove(1,dw_2.RowCount(),Primary!,dw_2,1,Delete!)

cb_aplicar.Enabled	= True
cb_aplicatodo.Enabled	= True
lb_modifica = TRUE
dw_3.SetFocus()
end event

type st_5 from statictext within w_mant_deta_ordenproceso_comercial
integer x = 32
integer y = 32
integer width = 2999
integer height = 488
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

