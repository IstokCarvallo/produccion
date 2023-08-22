$PBExportHeader$w_mant_mues_planilla_control_proc.srw
forward
global type w_mant_mues_planilla_control_proc from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_planilla_control_proc
end type
type st_3 from statictext within w_mant_mues_planilla_control_proc
end type
type st_4 from statictext within w_mant_mues_planilla_control_proc
end type
type em_fecha from editmask within w_mant_mues_planilla_control_proc
end type
type em_fechater from editmask within w_mant_mues_planilla_control_proc
end type
type st_5 from statictext within w_mant_mues_planilla_control_proc
end type
type dw_2 from uo_dw within w_mant_mues_planilla_control_proc
end type
type ddlb_1 from dropdownlistbox within w_mant_mues_planilla_control_proc
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_planilla_control_proc
end type
type gb_4 from groupbox within w_mant_mues_planilla_control_proc
end type
end forward

global type w_mant_mues_planilla_control_proc from w_mant_tabla
integer width = 4402
integer height = 2020
string title = "Planilla Control Proceso"
st_1 st_1
st_3 st_3
st_4 st_4
em_fecha em_fecha
em_fechater em_fechater
st_5 st_5
dw_2 dw_2
ddlb_1 ddlb_1
uo_selplanta uo_selplanta
gb_4 gb_4
end type
global w_mant_mues_planilla_control_proc w_mant_mues_planilla_control_proc

type variables
DataWindowChild idwc_planta

uo_plantadesp 	 iuo_plantadesp


Long il_fila_e = 1
end variables

forward prototypes
protected function integer wf_modifica ()
protected function boolean wf_actualiza_db ()
public function integer nroaviso (long al_orden)
end prototypes

protected function integer wf_modifica ();if dw_2.accepttext() = -1 then return -1
if (dw_2.modifiedcount() + dw_2.deletedcount()) > 0 then return 0
return 1
end function

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_2.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_2.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_2.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function integer nroaviso (long al_orden);Integer	li_Tippro, li_Aviso

li_Tippro	=	integer(istr_mant.argumento[2])

UPDATE dbo.spro_avisoproductores
   SET orpa_nroavi = 0
 WHERE 1 = 2;

SELECT	IsNull(Max(orpa_nroavi),0) + 1
	INTO	:li_Aviso
	FROM	dbo.spro_avisoproductores
	WHERE	plde_codigo	=	:uo_SelPlanta.Codigo
	AND	orpr_tipord		=	:li_tippro
	AND	orpr_numero	=	:al_orden;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Aviso Productores.")
   RETURN 0	
ELSEIF SQLCA.SQLCode = 100 THEN
	RETURN 0
END IF

IF li_Aviso > 99 THEN
	MessageBox("Atención","Los números de avisos superaron el rango permitido.")
	RETURN 0
END IF	

RETURN li_Aviso
end function

event ue_imprimir;//
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	dw_1.SetRedraw(True)
	ll_fila	= dw_1.Retrieve(uo_SelPlanta.Codigo,Integer(Istr_Mant.Argumento[2]),+ &
									Date(Mid(istr_mant.Argumento[3], 1, 10)),Date(Mid(istr_mant.Argumento[4], 1, 10)))
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
	ELSE	
		MessageBox(	"Atención", "No Existe Información.", Information!)
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN
	Close(This)
END IF
end event

on w_mant_mues_planilla_control_proc.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_3=create st_3
this.st_4=create st_4
this.em_fecha=create em_fecha
this.em_fechater=create em_fechater
this.st_5=create st_5
this.dw_2=create dw_2
this.ddlb_1=create ddlb_1
this.uo_selplanta=create uo_selplanta
this.gb_4=create gb_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.em_fecha
this.Control[iCurrent+5]=this.em_fechater
this.Control[iCurrent+6]=this.st_5
this.Control[iCurrent+7]=this.dw_2
this.Control[iCurrent+8]=this.ddlb_1
this.Control[iCurrent+9]=this.uo_selplanta
this.Control[iCurrent+10]=this.gb_4
end on

on w_mant_mues_planilla_control_proc.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.em_fecha)
destroy(this.em_fechater)
destroy(this.st_5)
destroy(this.dw_2)
destroy(this.ddlb_1)
destroy(this.uo_selplanta)
destroy(this.gb_4)
end on

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	
	istr_Mant.Argumento[2]  =	"4"
	istr_Mant.Argumento[3]  =	String(Today(),'dd/mm/yyyy')
	istr_mant.Argumento[4]  =  String(Today(),'dd/mm/yyyy')
	
	ddlb_1.SelectItem(1)
	dw_2.SetTransObject(Sqlca)
	
	em_fecha.Text 		=	String(Today(),'dd/mm/yyyy')
	em_fechater.Text	=  String(Today(),'dd/mm/yyyy')
	em_fecha.SetFocus()
	
	buscar	= "Código planta:Nplde_codigo,Tipo Orden:Norpr_tipord"
	ordenar	= "Código planta:plde_codigo,Tipo Orden:orpr_tipord"
End If
end event

event ue_antesguardar;String   ls_fechaini, ls_horaini,ls_fechater, ls_horater,ls_null
Integer	li_cont,li_cont1,ll_fila
String	ls_mensaje,ls_colu[],ls_colu1[]

SetNull(ls_null)	
Message.DoubleParm = 0

FOR ll_fila = 1 To dw_2.RowCount() 
	IF Isnull(dw_2.Object.orpa_fecavi[ll_Fila]) OR dw_2.Object.orpa_fecavi[ll_fila] = Date(ls_Null) THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nFecha Aviso"
		ls_colu[li_cont]	= "orpa_fecavi"
	END IF
		
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + "en el Aviso Nº " + &
		           String(dw_2.Object.orpa_nroavi[ll_fila],"00"), StopSign!, Ok!)
		dw_2.SetColumn(ls_colu[li_cont])
		dw_2.SetRow(ll_fila)
		dw_2.SetFocus()
		Message.DoubleParm = -1
		RETURN
	END IF
NEXT
end event

event ue_listo();//
end event

event resize;call super::resize;dw_1.Height				= This.WorkSpaceHeight() - dw_2.Height - dw_1.y - 41

dw_2.x					= (dw_1.width - dw_2.width) / 2
dw_2.y					= dw_1.y + dw_1.Height + 30


end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_planilla_control_proc
integer x = 32
integer y = 416
integer width = 3552
integer height = 1344
integer taborder = 70
string dataobject = "dw_mues_spro_planilla_control_proc"
boolean hscrollbar = true
end type

event dw_1::clicked;Integer 	li_aviso, ll_fila=0

IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)

	IF dw_2.RowCount() > 0 THEN
		IF dw_2.ModifiedCount() > 0 THEN
			IF MessageBox("Datos Pendientes de Grabación","Desea grabar los avisos",Question!,YesNo!,1) = 1 THEN
				Parent.TriggerEvent("ue_guardar")
				IF Message.DoubleParm = -1 THEN RETURN
			END IF
		END IF
	END IF
	
	li_aviso = NroAviso(dw_1.Object.orpr_numero[Row])
	dw_2.Reset()	
	
	IF li_Aviso > 1 THEN dw_2.Retrieve(uo_SelPlanta.Codigo,Integer(Istr_Mant.Argumento[2]), dw_1.Object.orpr_numero[Row])			
	
	IF li_Aviso > 0 THEN
		pb_grabar.Enabled	=	TRUE
		il_fila_e = dw_2.InsertRow(0)
		dw_2.ScrollToRow(il_fila_e)
		dw_2.SetRow(il_fila_e)
		dw_2.SetItem(il_fila_e,"plde_codigo",uo_SelPlanta.Codigo)
		dw_2.SetItem(il_fila_e,"orpr_tipord",Integer(istr_Mant.Argumento[2]))
		dw_2.SetItem(il_fila_e,"orpr_numero",dw_1.Object.orpr_numero[Row])
		dw_2.SetItem(il_fila_e,"orpa_fecavi", Today())
		dw_2.SetItem(il_fila_e,"orpa_horavi", Now())
		
		dw_2.SetItem(il_fila_e,"orpa_nroavi",li_Aviso)		
      
		dw_2.SetFocus()
	END IF 
END IF
end event

event dw_1::doubleclicked;//
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_planilla_control_proc
integer x = 32
integer width = 3552
integer height = 324
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_planilla_control_proc
integer x = 4037
integer y = 184
integer taborder = 60
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_planilla_control_proc
integer x = 4037
integer y = 480
integer taborder = 0
end type

event pb_nuevo::clicked;dw_1.Reset()
dw_2.Reset()

Parent.TriggerEvent("ue_nuevo")

end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_planilla_control_proc
boolean visible = false
integer x = 3077
integer y = 1416
integer taborder = 80
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_planilla_control_proc
boolean visible = false
integer x = 3077
integer y = 1592
integer taborder = 100
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_planilla_control_proc
integer x = 4037
integer y = 660
integer taborder = 110
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_planilla_control_proc
boolean visible = false
integer x = 2985
integer y = 1164
integer taborder = 10
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_planilla_control_proc
integer x = 4041
integer y = 1012
integer taborder = 120
end type

type st_1 from statictext within w_mant_mues_planilla_control_proc
integer x = 613
integer y = 132
integer width = 210
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_planilla_control_proc
integer x = 1911
integer y = 132
integer width = 393
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Tipo Proceso"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_planilla_control_proc
integer x = 613
integer y = 252
integer width = 389
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Fecha Inicio"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_planilla_control_proc
string tag = "Fecha Inicio"
integer x = 987
integer y = 248
integer width = 480
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
long calendarbackcolor = 15780518
end type

event modified;String ls_Null
SetNull(ls_Null)

IF Date(Mid(THIS.Text, 1, 10)) > Date(istr_Mant.Argumento[4]) THEN
	MessageBox("Error", "Fecha de inicio no debe ser posterior a la Fecha de término." + &
					"~r~rIngrese otra fecha de inicio.")			
	THIS.Text	=	ls_Null
	istr_Mant.Argumento[3]	 = ""
	RETURN 0
ELSE
	istr_Mant.Argumento[3]	 = This.text
END IF

IF istr_Mant.Argumento[2] <> "" AND istr_Mant.Argumento[3] <> "" AND &
   istr_Mant.Argumento[4] <> "" THEN
	pb_lectura.Enabled		=	True
END IF		

end event

type em_fechater from editmask within w_mant_mues_planilla_control_proc
string tag = "Fecha Termino"
integer x = 2377
integer y = 248
integer width = 480
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
long calendarbackcolor = 15780518
end type

event modified;String ls_Null
SetNull(ls_Null)
IF Date(Mid(THIS.Text, 1, 10)) < Date(istr_Mant.Argumento[3]) THEN
	MessageBox("Error", "Fecha de Termino No debe ser Anterior a la Fecha de Inicio." + &
					"~r~rIngrese otra Fecha de Termino.")			
	THIS.Text	=	ls_Null
	istr_Mant.Argumento[4]	 = ""
	RETURN 0
ELSE
	istr_Mant.Argumento[4]	 = This.text
END IF

IF istr_Mant.Argumento[2] <> "" AND istr_Mant.Argumento[3] <> "" AND &
   istr_Mant.Argumento[4] <> "" THEN
	pb_lectura.Enabled		=	True
END IF		
end event

type st_5 from statictext within w_mant_mues_planilla_control_proc
integer x = 1911
integer y = 252
integer width = 453
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Fecha Término"
boolean focusrectangle = false
end type

type dw_2 from uo_dw within w_mant_mues_planilla_control_proc
integer x = 32
integer y = 1080
integer width = 2779
integer height = 584
integer taborder = 90
boolean bringtotop = true
boolean titlebar = true
string title = "Aviso Proceso"
string dataobject = "dw_mues_spro_aviso_proc"
boolean vscrollbar = false
borderstyle borderstyle = stylebox!
end type

event itemchanged;String ls_Columna,ls_Null
Date   ld_fecha

SetNull(ls_Null)
SetNull(ld_fecha)

CHOOSE CASE dwo.name
		
	CASE "orpa_fecavi"
		IF Date(Mid(data, 1, 10)) < Date(istr_Mant.Argumento[3]) THEN
			MessageBox("Error", "Fecha de Aviso no debe ser Anterior a Inicio ." + &
							"~r~rIngrese otra Fecha de Aviso.")			
			dw_2.SetItem(row,"orpa_fecavi",Date(ls_Null))	
			RETURN 1
		END IF
	
END CHOOSE		
		
end event

event itemerror;call super::itemerror;Return 1
end event

event clicked;//
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()


end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila_e)
	ScrolltoRow(il_fila_e)
end if
end event

type ddlb_1 from dropdownlistbox within w_mant_mues_planilla_control_proc
string tag = "Tipo proceso"
integer x = 2377
integer y = 132
integer width = 599
integer height = 264
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean sorted = false
boolean vscrollbar = true
string item[] = {"Orden Proceso","Re-Proceso","","","","","","","","","","",""}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Index = 1 THEN	
	istr_mant.argumento[2] = '4'
ELSE
	istr_mant.argumento[2] = '5'
END IF	


IF istr_Mant.Argumento[2] <> "" AND istr_Mant.Argumento[3] <> "" AND &
   istr_Mant.Argumento[4] <> "" THEN
	pb_lectura.Enabled		=	True
END IF		



end event

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_planilla_control_proc
integer x = 987
integer y = 124
integer height = 100
integer taborder = 50
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type gb_4 from groupbox within w_mant_mues_planilla_control_proc
boolean visible = false
integer x = 128
integer y = 72
integer width = 2487
integer height = 324
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

