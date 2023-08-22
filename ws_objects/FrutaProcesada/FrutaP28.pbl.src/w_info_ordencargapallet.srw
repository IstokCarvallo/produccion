$PBExportHeader$w_info_ordencargapallet.srw
$PBExportComments$Proceso de Cierre Mensual.
forward
global type w_info_ordencargapallet from w_para_informes
end type
type dw_1 from datawindow within w_info_ordencargapallet
end type
type uo_planta from uo_seleccion_plantas within w_info_ordencargapallet
end type
type uo_clientes from uo_seleccion_clientesprod within w_info_ordencargapallet
end type
type st_1 from statictext within w_info_ordencargapallet
end type
type st_2 from statictext within w_info_ordencargapallet
end type
type gb_3 from groupbox within w_info_ordencargapallet
end type
type st_3 from statictext within w_info_ordencargapallet
end type
type em_orden from editmask within w_info_ordencargapallet
end type
type cb_1 from commandbutton within w_info_ordencargapallet
end type
type st_4 from statictext within w_info_ordencargapallet
end type
type rb_general from radiobutton within w_info_ordencargapallet
end type
type rb_clasificado from radiobutton within w_info_ordencargapallet
end type
type gb_4 from groupbox within w_info_ordencargapallet
end type
type st_7 from statictext within w_info_ordencargapallet
end type
type st_5 from statictext within w_info_ordencargapallet
end type
end forward

global type w_info_ordencargapallet from w_para_informes
integer width = 3566
integer height = 1544
string title = "Informe Pallet Por Orden Embarque"
boolean controlmenu = false
event ue_validapassword ( )
event ue_imprimir ( )
dw_1 dw_1
uo_planta uo_planta
uo_clientes uo_clientes
st_1 st_1
st_2 st_2
gb_3 gb_3
st_3 st_3
em_orden em_orden
cb_1 cb_1
st_4 st_4
rb_general rb_general
rb_clasificado rb_clasificado
gb_4 gb_4
st_7 st_7
st_5 st_5
end type
global w_info_ordencargapallet w_info_ordencargapallet

type variables
str_mant istr_mant
Str_busqueda	istr_busq

integer ii_tipo = -1

DataWindowChild	idwc_planta

uo_plantadesp			iuo_plantadesp
end variables

forward prototypes
public function boolean noexistenumero (string columna, integer tipo)
public subroutine buscaordencarga ()
end prototypes

event ue_imprimir();SetPointer(HourGlass!)

Long		fila,ll_orden
String 	ls_transporte


istr_info.titulo	= "ORDEN DE CARGA PALLET"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_ordencargageneral"

vinf.dw_1.SetTransObject(sqlca)

ls_transporte =dw_1.Object.tipo_transp[1]

ll_orden = Long(em_orden.text)

fila = vinf.dw_1.Retrieve(ls_transporte,ll_orden,  uo_clientes.codigo,uo_planta.codigo,ii_tipo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

public function boolean noexistenumero (string columna, integer tipo);Long		li_Numero, li_NumOcen
Integer	li_null, li_Cliente
String	ls_tipoEmb

SetNull (li_null)

ls_tipoEmb	= 	dw_1.Object.tipo_transp[1]
li_Numero	= 	Long(em_orden.text)
li_Cliente	=	uo_clientes.codigo

CHOOSE CASE tipo
	CASE 1
		li_Numero = Long(columna)
		
	CASE 2
		ls_TipoEmb = columna
		
END CHOOSE

IF IsNull(ls_tipoEmb) = False OR IsNull(li_Numero) = False THEN
	
	SELECT	ocen_numero
		INTO	:li_NumOcen
		FROM	dba.ordencargaenca
		WHERE	nave_tipotr = :ls_TipoEmb
		AND	ocen_numero = :li_numero
		AND   clie_codigo = :li_Cliente;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla ORDENCARGAENCA")	
		RETURN TRUE
      
	ELSEIF sqlca.SQLCode = 0 THEN
		RETURN FALSE
	ELSE 
		
		dw_1.Object.ocen_numero[1]	=	 li_null
		RETURN TRUE
	END IF
END IF

end function

public subroutine buscaordencarga ();istr_busq.argum[1]	=	dw_1.Object.tipo_transp[1]
istr_busq.argum[10]	=	String(uo_clientes.codigo)

OpenWithParm(w_busc_ordencargaenca,istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	dw_1.SetItem(1,"tipo_transp",istr_busq.argum[1])
	em_orden.text =istr_busq.argum[2]
	RETURN
ELSE
	RETURN
END IF

end subroutine

on w_info_ordencargapallet.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.uo_planta=create uo_planta
this.uo_clientes=create uo_clientes
this.st_1=create st_1
this.st_2=create st_2
this.gb_3=create gb_3
this.st_3=create st_3
this.em_orden=create em_orden
this.cb_1=create cb_1
this.st_4=create st_4
this.rb_general=create rb_general
this.rb_clasificado=create rb_clasificado
this.gb_4=create gb_4
this.st_7=create st_7
this.st_5=create st_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.uo_planta
this.Control[iCurrent+3]=this.uo_clientes
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.gb_3
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.em_orden
this.Control[iCurrent+9]=this.cb_1
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.rb_general
this.Control[iCurrent+12]=this.rb_clasificado
this.Control[iCurrent+13]=this.gb_4
this.Control[iCurrent+14]=this.st_7
this.Control[iCurrent+15]=this.st_5
end on

on w_info_ordencargapallet.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.uo_planta)
destroy(this.uo_clientes)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.gb_3)
destroy(this.st_3)
destroy(this.em_orden)
destroy(this.cb_1)
destroy(this.st_4)
destroy(this.rb_general)
destroy(this.rb_clasificado)
destroy(this.gb_4)
destroy(this.st_7)
destroy(this.st_5)
end on

event open;call super::open;
//dw_1.Object.clie_codigo[1] = gi_codexport
dw_1.Object.tipo_transp[1] = 'M'


dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

uo_clientes.codigo = gi_codexport
uo_planta.codigo = gi_codplanta

uo_clientes.cbx_todos.checked=false
uo_planta.cbx_todos.checked=false

uo_clientes.cbx_todos.visible=false
uo_planta.cbx_todos.visible=false

uo_clientes.cbx_consolida.visible=false
uo_planta.cbx_consolida.visible=false

uo_clientes.dw_seleccion.object.codigo[1] = gi_codexport
uo_planta.dw_seleccion.object.codigo[1] = gi_codplanta

//uo_clientes.dw_seleccion.enabled=true
//uo_planta.dw_seleccion.enabled=true

uo_clientes.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
uo_planta.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	

end event

type st_computador from w_para_informes`st_computador within w_info_ordencargapallet
end type

type st_usuario from w_para_informes`st_usuario within w_info_ordencargapallet
end type

type st_temporada from w_para_informes`st_temporada within w_info_ordencargapallet
end type

type p_logo from w_para_informes`p_logo within w_info_ordencargapallet
end type

type st_titulo from w_para_informes`st_titulo within w_info_ordencargapallet
integer x = 242
integer width = 2743
string text = "Informe Pallet Por Orden Embarque"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_ordencargapallet
integer x = 3150
integer y = 604
string picturename = "\desarrollo\bmp\aceptae.bmp"
string disabledname = "\desarrollo\bmp\aceptad.bmp"
end type

event pb_acepta::clicked;String	ls_Trans
Integer	li_cliente,li_Planta
Long		li_NroOrden

SetPointer(HourGlass!)

IF IsNull(dw_1.Object.tipo_transp[1]) OR dw_1.Object.tipo_transp[1] = '' THEN
	MessageBox("Atención", "Debe Ingresar Tipo de Transporte Previamente ",Exclamation!)
	RETURN
ELSEIF IsNull(Long(em_orden.text)) OR Long(em_orden.text) = 0 THEN
 MessageBox("Atención", "Debe Ingresar Número de Orden Previamente ",Exclamation!)
	RETURN
ELSEIF IsNull(uo_clientes.codigo) OR uo_clientes.codigo = 0 THEN
 MessageBox("Atención", "Debe Ingresar Cliente Previamente ",Exclamation!)
	RETURN
ELSEIF IsNull(uo_planta.codigo)  OR uo_planta.codigo = 0 THEN
 MessageBox("Atención", "Debe Ingresar Planta Previamente ",Exclamation!)
	RETURN
END IF

ls_Trans		=	dw_1.Object.tipo_transp[1]
li_cliente	=	uo_clientes.codigo
li_NroOrden	=	Long(em_orden.text)
li_Planta	=	uo_planta.codigo
					
Parent.TriggerEvent("ue_imprimir")
end event

type pb_salir from w_para_informes`pb_salir within w_info_ordencargapallet
integer x = 3150
integer y = 880
end type

type dw_1 from datawindow within w_info_ordencargapallet
integer x = 2149
integer y = 600
integer width = 690
integer height = 88
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_param_transporte"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Null
dw_1.accepttext()
SetNull(li_Null)

CHOOSE CASE dwo.Name
	
	CASE "tipo_transp"
	   IF NoExisteNumero(data, 2) THEN
			dw_1.SetItem(1, "ocen_numero", Long(li_Null))
			RETURN 1
		END IF
				
END CHOOSE


end event

event itemerror;RETURN 1
end event

type uo_planta from uo_seleccion_plantas within w_info_ordencargapallet
integer x = 672
integer y = 712
integer width = 974
integer taborder = 30
boolean bringtotop = true
end type

on uo_planta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_clientes from uo_seleccion_clientesprod within w_info_ordencargapallet
integer x = 672
integer y = 524
integer width = 974
integer taborder = 40
boolean bringtotop = true
end type

on uo_clientes.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_1 from statictext within w_info_ordencargapallet
integer x = 357
integer y = 628
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_ordencargapallet
integer x = 357
integer y = 816
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_ordencargapallet
integer x = 297
integer y = 464
integer width = 2619
integer height = 524
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_3 from statictext within w_info_ordencargapallet
integer x = 1659
integer y = 816
integer width = 466
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Nº Orden Carga"
boolean focusrectangle = false
end type

type em_orden from editmask within w_info_ordencargapallet
integer x = 2167
integer y = 796
integer width = 439
integer height = 80
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "#"
end type

event modified;Integer li_null

IF NoExisteNumero(em_orden.text, 1) THEN
	em_orden.text=String(li_Null)
	RETURN 1
END IF
end event

type cb_1 from commandbutton within w_info_ordencargapallet
integer x = 2674
integer y = 792
integer width = 101
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;BuscaOrdenCarga()

end event

type st_4 from statictext within w_info_ordencargapallet
integer x = 1659
integer y = 620
integer width = 471
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Tipo Transporte"
boolean focusrectangle = false
end type

type rb_general from radiobutton within w_info_ordencargapallet
integer x = 672
integer y = 1060
integer width = 402
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "General"
boolean checked = true
end type

event clicked;if rb_general.checked then
	ii_tipo=-1
	rb_clasificado.checked=false
end if
end event

type rb_clasificado from radiobutton within w_info_ordencargapallet
integer x = 1102
integer y = 1060
integer width = 402
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Clasificado"
end type

event clicked;if rb_clasificado.checked then
	ii_tipo=1
	rb_general.checked=false
end if
end event

type gb_4 from groupbox within w_info_ordencargapallet
integer x = 297
integer y = 984
integer width = 2619
integer height = 192
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_7 from statictext within w_info_ordencargapallet
integer x = 242
integer y = 440
integer width = 2743
integer height = 800
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_ordencargapallet
integer x = 357
integer y = 1072
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Tipo"
boolean focusrectangle = false
end type

