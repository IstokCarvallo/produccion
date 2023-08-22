$PBExportHeader$w_info_planilla_detalle_lotes.srw
$PBExportComments$Ventana de Informe de Control Calidad de Daños y Defectos
forward
global type w_info_planilla_detalle_lotes from w_para_informes
end type
type st_3 from statictext within w_info_planilla_detalle_lotes
end type
type st_4 from statictext within w_info_planilla_detalle_lotes
end type
type pb_1 from picturebutton within w_info_planilla_detalle_lotes
end type
type rb_2 from radiobutton within w_info_planilla_detalle_lotes
end type
type gb_3 from groupbox within w_info_planilla_detalle_lotes
end type
type st_5 from statictext within w_info_planilla_detalle_lotes
end type
type rb_1 from radiobutton within w_info_planilla_detalle_lotes
end type
type dw_lotes from datawindow within w_info_planilla_detalle_lotes
end type
type st_2 from statictext within w_info_planilla_detalle_lotes
end type
type dw_planta from datawindow within w_info_planilla_detalle_lotes
end type
type em_recepcion from editmask within w_info_planilla_detalle_lotes
end type
type st_6 from statictext within w_info_planilla_detalle_lotes
end type
type dw_cliente from datawindow within w_info_planilla_detalle_lotes
end type
type cbx_1 from checkbox within w_info_planilla_detalle_lotes
end type
end forward

global type w_info_planilla_detalle_lotes from w_para_informes
integer width = 3310
integer height = 2048
boolean resizable = false
event ue_recuperadatos ( )
event ue_imprimir ( )
st_3 st_3
st_4 st_4
pb_1 pb_1
rb_2 rb_2
gb_3 gb_3
st_5 st_5
rb_1 rb_1
dw_lotes dw_lotes
st_2 st_2
dw_planta dw_planta
em_recepcion em_recepcion
st_6 st_6
dw_cliente dw_cliente
cbx_1 cbx_1
end type
global w_info_planilla_detalle_lotes w_info_planilla_detalle_lotes

type variables
datawindowchild  	idwc_planta, idwc_cliente

Integer				ii_CuantosLotes

str_busqueda		istr_busq
str_mant				istr_mant
end variables

forward prototypes
public function boolean noexistecliente (integer cliente)
public function boolean noexisteplanta (integer planta, integer cliente)
end prototypes

event ue_recuperadatos();Datawindowchild  ldwc_lotes
Long	ll_fila, respuesta

DO
	dw_lotes.GetChild("vari_codigo", ldwc_lotes)
	ldwc_lotes.SetTransObject(SqlCa)
	ldwc_lotes.Retrieve(0)

	dw_lotes.SetTransObject(Sqlca)
	ll_fila	= dw_lotes.Retrieve(dw_planta.Object.plde_codigo[1],&
										  Integer(Istr_busq.Argum[2]),&
										  Integer(istr_busq.Argum[3]),&
										  dw_cliente.Object.clie_codigo[1])
										  

	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila = 0 THEN
		Messagebox("Error","No Existe Número de Recepción Para Esta Planta")
   END IF									
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dbo.clientesprod
WHERE		clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

public function boolean noexisteplanta (integer planta, integer cliente);Integer	codigo

	
	SELECT plde_codigo
	INTO	 :Codigo	
	FROM dbo.Plantadesp
	WHERE	plde_codigo	=	:planta;
//	  AND clie_codigo =  :cliente ;
	
	
	IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantas" )
			Return True			
	ELSEIF sqlca.SQLCode = 100 THEN
			messagebox("Atención","No Existe Código Seleccionado en Tabla ")			
			Return True						
	END IF

	Return False


end function

on w_info_planilla_detalle_lotes.create
int iCurrent
call super::create
this.st_3=create st_3
this.st_4=create st_4
this.pb_1=create pb_1
this.rb_2=create rb_2
this.gb_3=create gb_3
this.st_5=create st_5
this.rb_1=create rb_1
this.dw_lotes=create dw_lotes
this.st_2=create st_2
this.dw_planta=create dw_planta
this.em_recepcion=create em_recepcion
this.st_6=create st_6
this.dw_cliente=create dw_cliente
this.cbx_1=create cbx_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.pb_1
this.Control[iCurrent+4]=this.rb_2
this.Control[iCurrent+5]=this.gb_3
this.Control[iCurrent+6]=this.st_5
this.Control[iCurrent+7]=this.rb_1
this.Control[iCurrent+8]=this.dw_lotes
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.dw_planta
this.Control[iCurrent+11]=this.em_recepcion
this.Control[iCurrent+12]=this.st_6
this.Control[iCurrent+13]=this.dw_cliente
this.Control[iCurrent+14]=this.cbx_1
end on

on w_info_planilla_detalle_lotes.destroy
call super::destroy
destroy(this.st_3)
destroy(this.st_4)
destroy(this.pb_1)
destroy(this.rb_2)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.rb_1)
destroy(this.dw_lotes)
destroy(this.st_2)
destroy(this.dw_planta)
destroy(this.em_recepcion)
destroy(this.st_6)
destroy(this.dw_cliente)
destroy(this.cbx_1)
end on

event open;dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SqlCa)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1,"clie_codigo",gi_codexport)
istr_busq.argum[4] = String(gi_codexport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
idwc_planta.Retrieve()
dw_planta.InsertRow(0)

end event

type pb_excel from w_para_informes`pb_excel within w_info_planilla_detalle_lotes
integer x = 2944
integer y = 380
end type

type st_computador from w_para_informes`st_computador within w_info_planilla_detalle_lotes
integer x = 1573
integer y = 148
integer width = 1728
end type

type st_usuario from w_para_informes`st_usuario within w_info_planilla_detalle_lotes
integer x = 1573
integer width = 1728
end type

type st_temporada from w_para_informes`st_temporada within w_info_planilla_detalle_lotes
integer x = 1573
integer y = 4
integer width = 1728
end type

type p_logo from w_para_informes`p_logo within w_info_planilla_detalle_lotes
end type

type st_titulo from w_para_informes`st_titulo within w_info_planilla_detalle_lotes
integer width = 2432
string text = "Planilla Detalle de Recepción (Bins)"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_planilla_detalle_lotes
integer x = 2926
integer y = 752
integer taborder = 80
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
Long		ll_filaret, ll_fila, respuesta
String 	n_lote, tmp, command
Datawindowchild  ldwc_lotes

istr_info.titulo	= "LISTADO ANEXO DE PESAJE DE BINS"
istr_info.copias	= 1

IF dw_planta.Object.plde_codigo[dw_planta.GetRow()]	= 0   OR &
   IsNull(dw_planta.Object.plde_codigo[dw_planta.GetRow()]) THEN
	Messagebox("Mensaje","Debe Ingresar Código de Planta")
	Return 1
ELSE
	IF (Integer(em_recepcion.text) = 0 OR IsNull(Integer(em_recepcion.text))) AND NOT cbx_1.Checked THEN
      Messagebox("Mensaje","Debe Ingresar Nº Recepción")
	   Return 1
	ELSE
		istr_info.Multiple	=	TRUE
		OpenWithParm(vinf, istr_info)
		
		vinf.dw_1.DataObject = "dw_info_detalle_pesaje_bins_extarna"
		vinf.dw_1.SetTransObject(Sqlca)
		
		FOR ll_fila = 1 TO dw_lotes.RowCount()
			IF dw_lotes.IsSelected(ll_fila) THEN
				
				
				ll_filaret	= vinf.dw_1.Retrieve(dw_planta.Object.plde_codigo[1],&
											dw_lotes.Object.mfge_numero[ll_fila],&
											dw_cliente.Object.clie_codigo[1],&
											dw_lotes.object.lote_codigo[ll_fila])
				IF ll_filaret < 1 THEN 
					MessageBox("Error", "No existe información para informe del lote " + String(n_lote))
					Return 1
				ELSE
					F_Membrete(vinf.dw_1)
					ParamTemporada(gstr_paramtempo)
					vinf.dw_1.Modify("nombre_temporada.text = '" + gstr_paramtempo.nombre + "'")
					IF gs_Ambiente <> 'Windows' THEN
							F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
					
					END IF
					//vinf.dw_1.Print()
				END IF
			END IF
		NEXT
		
	END IF
END IF
SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_planilla_detalle_lotes
integer x = 2926
integer y = 1116
integer taborder = 90
end type

type st_3 from statictext within w_info_planilla_detalle_lotes
integer x = 302
integer y = 596
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_planilla_detalle_lotes
integer x = 302
integer y = 712
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nº Recepción"
boolean focusrectangle = false
end type

type pb_1 from picturebutton within w_info_planilla_detalle_lotes
integer x = 1193
integer y = 696
integer width = 101
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
boolean originalsize = true
end type

event clicked;Long	ll_fila
dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
idwc_planta.Retrieve()
dw_planta.InsertRow(0)

istr_busq.Argum[1]	=	String(dw_planta.Object.plde_codigo[1])
istr_busq.Argum[2]	=  '1'
istr_busq.Argum[3]	=  '3'
istr_busq.Argum[5]	=  '0'
istr_busq.Argum[4]   =  String(dw_cliente.Object.clie_codigo[1])//CLIENTE

IF dw_planta.Object.plde_codigo[1] >0 THEN

OpenWithParm(w_busc_spro_recepcion_lote, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[10] = String(1) THEN
	em_recepcion.Text	=	istr_busq.Argum[3]
	PARENT.TriggerEvent("ue_recuperadatos")

	FOR ll_fila	=	1 TO dw_lotes.RowCount()
		dw_lotes.SelectRow(ll_fila, TRUE)
	NEXT 

	IF ll_fila	>	1 THEN
 		rb_1.Checked = TRUE
	END IF 
END IF

ELSE
	Messagebox("Atención","Debe Seleccionar una Planta",exclamation!)
	dw_planta.SetFocus()
	RETURN
END IF














end event

type rb_2 from radiobutton within w_info_planilla_detalle_lotes
integer x = 1339
integer y = 940
integer width = 622
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desaplicar Todos"
end type

event clicked;Long	ll_fila
FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(ll_fila, False)
NEXT 
ii_CuantosLotes = 0
end event

type gb_3 from groupbox within w_info_planilla_detalle_lotes
integer x = 302
integer y = 848
integer width = 2318
integer height = 216
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Opciones"
borderstyle borderstyle = styleraised!
end type

type st_5 from statictext within w_info_planilla_detalle_lotes
integer x = 256
integer y = 824
integer width = 2432
integer height = 1060
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_1 from radiobutton within w_info_planilla_detalle_lotes
integer x = 640
integer y = 940
integer width = 622
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Aplicar Todos"
end type

event clicked;Long	ll_fila
FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(ll_fila, True)
NEXT 
ii_CuantosLotes = dw_lotes.RowCount()
end event

type dw_lotes from datawindow within w_info_planilla_detalle_lotes
integer x = 293
integer y = 1088
integer width = 2341
integer height = 744
boolean bringtotop = true
boolean titlebar = true
string title = "Lotes"
string dataobject = "dw_mues_spro_lotesfrutagranel_informes"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
boolean livescroll = true
borderstyle borderstyle = styleraised!
end type

event clicked;SelectRow(row, NOT IsSelected(Row))


end event

type st_2 from statictext within w_info_planilla_detalle_lotes
integer x = 256
integer y = 400
integer width = 2432
integer height = 420
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_planilla_detalle_lotes
integer x = 782
integer y = 568
integer width = 873
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Long  ll_Null

IF NoExistePlanta(Integer(Data),gi_codexport) THEN
	This.SetItem(Row, "plde_codigo",IsNull(ll_Null))
	Return 1
ELSE
	istr_busq.Argum[1]	= Data
	//em_recepcion.SetFocus()
	em_recepcion.text=''
	dw_lotes.Reset()

END IF
	

end event

event itemerror;Return 1
end event

type em_recepcion from editmask within w_info_planilla_detalle_lotes
integer x = 782
integer y = 696
integer width = 370
integer height = 80
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;long ll_fila
istr_busq.Argum[1]	=	' '
istr_busq.Argum[2]	=  ' '
istr_busq.Argum[3]	=  ' '
istr_busq.Argum[5]	=  ' '

istr_busq.Argum[1]	=	String(dw_planta.Object.plde_codigo[1])
istr_busq.Argum[2]	=  '1'
istr_busq.Argum[3]	=  em_recepcion.Text
istr_busq.Argum[5]	=  '0'
istr_busq.Argum[4]	=  String(dw_cliente.Object.clie_codigo[1])



Parent.TriggerEvent("ue_recuperadatos")

FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(ll_fila, TRUE)
NEXT 

ii_CuantosLotes = dw_lotes.RowCount()

IF ll_fila	>	1 THEN
 	rb_1.Checked = True
END IF 

end event

type st_6 from statictext within w_info_planilla_detalle_lotes
integer x = 302
integer y = 468
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_planilla_detalle_lotes
integer x = 782
integer y = 444
integer width = 1157
integer height = 100
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Long  ll_Null

IF NoExisteCliente(Integer(Data)) THEN
	This.SetItem(Row, "clie_codigo",IsNull(ll_Null))
	Return 1
ELSE
	istr_busq.Argum[4]	= Data
	dw_planta.SetFocus()
END IF
	
end event

event itemerror;RETURN 1
end event

type cbx_1 from checkbox within w_info_planilla_detalle_lotes
integer x = 1403
integer y = 696
integer width = 402
integer height = 80
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
end type

event clicked;long ll_fila
istr_busq.Argum[1]	=	' '
istr_busq.Argum[2]	=  ' '
istr_busq.Argum[3]	=  ' '
istr_busq.Argum[5]	=  ' '
IF THIS.Checked THEN
	
	istr_busq.Argum[1]	=	String(dw_planta.Object.plde_codigo[1])
	istr_busq.Argum[2]	=  '1'
	istr_busq.Argum[3]	= '-1'
	istr_busq.Argum[5]	=  '0'
	istr_busq.Argum[4]	=  String(dw_cliente.Object.clie_codigo[1])
	
	em_recepcion.Enabled 	= 	FALSE
	em_recepcion.BackColor	= 	RGB(192,192,192)
	pb_1.Enabled				=	FALSE
	THIS.Checked 				=	TRUE
	
	Parent.TriggerEvent("ue_recuperadatos")
	
	FOR ll_fila	=	1 TO dw_lotes.RowCount()
		dw_lotes.SelectRow(ll_fila, TRUE)
	NEXT 
	
	ii_CuantosLotes = dw_lotes.RowCount()
	
	IF ll_fila	>	1 THEN
		rb_1.Checked = True
	END IF 
ELSE
	em_recepcion.Enabled 	= 	TRUE
	em_recepcion.BackColor	= 	RGB(255, 255, 255)
	pb_1.Enabled				=	TRUE
	THIS.Checked 				=	FALSE
END IF
end event

