$PBExportHeader$w_info_conspalletsordenembarque.srw
$PBExportComments$Proceso de Cierre Mensual.
forward
global type w_info_conspalletsordenembarque from w_para_informes
end type
type uo_planta from uo_seleccion_plantas within w_info_conspalletsordenembarque
end type
type uo_clientes from uo_seleccion_clientesprod within w_info_conspalletsordenembarque
end type
type st_1 from statictext within w_info_conspalletsordenembarque
end type
type st_2 from statictext within w_info_conspalletsordenembarque
end type
type cb_1 from commandbutton within w_info_conspalletsordenembarque
end type
type rb_general from radiobutton within w_info_conspalletsordenembarque
end type
type rb_clasificado from radiobutton within w_info_conspalletsordenembarque
end type
type gb_4 from groupbox within w_info_conspalletsordenembarque
end type
type st_5 from statictext within w_info_conspalletsordenembarque
end type
type st_6 from statictext within w_info_conspalletsordenembarque
end type
type dw_camara from datawindow within w_info_conspalletsordenembarque
end type
type gb_3 from groupbox within w_info_conspalletsordenembarque
end type
type st_7 from statictext within w_info_conspalletsordenembarque
end type
type st_4 from statictext within w_info_conspalletsordenembarque
end type
type dw_1 from datawindow within w_info_conspalletsordenembarque
end type
type st_3 from statictext within w_info_conspalletsordenembarque
end type
type em_orden from editmask within w_info_conspalletsordenembarque
end type
type cbx_camara from checkbox within w_info_conspalletsordenembarque
end type
end forward

global type w_info_conspalletsordenembarque from w_para_informes
integer width = 3625
integer height = 1700
string title = "Informe Consolidado de Carga Pallet"
boolean controlmenu = false
event ue_validapassword ( )
event ue_imprimir ( )
uo_planta uo_planta
uo_clientes uo_clientes
st_1 st_1
st_2 st_2
cb_1 cb_1
rb_general rb_general
rb_clasificado rb_clasificado
gb_4 gb_4
st_5 st_5
st_6 st_6
dw_camara dw_camara
gb_3 gb_3
st_7 st_7
st_4 st_4
dw_1 dw_1
st_3 st_3
em_orden em_orden
cbx_camara cbx_camara
end type
global w_info_conspalletsordenembarque w_info_conspalletsordenembarque

type variables
str_mant istr_mant
Str_busqueda	istr_busq

integer ii_tipo = -1
long il_camara = 0 

DataWindowChild	idwc_planta
datawindowchild idwc_camara

uo_plantadesp			iuo_plantadesp
end variables

forward prototypes
public function boolean noexistenumero (string columna, integer tipo)
public subroutine buscaordencarga ()
end prototypes

event ue_imprimir();SetPointer(HourGlass!)

Long		fila,ll_orden,ll_camara
String ls_transporte,ls_nombrecamara = 'Todas'


istr_info.titulo	= "CONSOLIDADO DE CARGA PALLET"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_conspalletsordenembarque"

vinf.dw_1.SetTransObject(sqlca)

ls_transporte =dw_1.Object.tipo_transp[1] 

ll_orden = Long(em_orden.text)

if il_camara = 0 then
	
   ll_camara = dw_camara.object.cama_codigo[1]
	
	select cama_nombre into :ls_nombrecamara
   from dba.camarasbode
   where plde_codigo=:uo_planta.codigo
   and cama_codigo = :ll_camara
   using sqlca;

else
	ll_camara = il_camara

end if

fila = vinf.dw_1.Retrieve(ls_transporte,ll_orden,  uo_clientes.codigo,uo_planta.codigo,ii_tipo,ll_camara)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("camara.text = 'Camara "+ls_nombrecamara+ "'")

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
		li_Numero = Integer(columna)
		
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

on w_info_conspalletsordenembarque.create
int iCurrent
call super::create
this.uo_planta=create uo_planta
this.uo_clientes=create uo_clientes
this.st_1=create st_1
this.st_2=create st_2
this.cb_1=create cb_1
this.rb_general=create rb_general
this.rb_clasificado=create rb_clasificado
this.gb_4=create gb_4
this.st_5=create st_5
this.st_6=create st_6
this.dw_camara=create dw_camara
this.gb_3=create gb_3
this.st_7=create st_7
this.st_4=create st_4
this.dw_1=create dw_1
this.st_3=create st_3
this.em_orden=create em_orden
this.cbx_camara=create cbx_camara
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_planta
this.Control[iCurrent+2]=this.uo_clientes
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.cb_1
this.Control[iCurrent+6]=this.rb_general
this.Control[iCurrent+7]=this.rb_clasificado
this.Control[iCurrent+8]=this.gb_4
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.st_6
this.Control[iCurrent+11]=this.dw_camara
this.Control[iCurrent+12]=this.gb_3
this.Control[iCurrent+13]=this.st_7
this.Control[iCurrent+14]=this.st_4
this.Control[iCurrent+15]=this.dw_1
this.Control[iCurrent+16]=this.st_3
this.Control[iCurrent+17]=this.em_orden
this.Control[iCurrent+18]=this.cbx_camara
end on

on w_info_conspalletsordenembarque.destroy
call super::destroy
destroy(this.uo_planta)
destroy(this.uo_clientes)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.cb_1)
destroy(this.rb_general)
destroy(this.rb_clasificado)
destroy(this.gb_4)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.dw_camara)
destroy(this.gb_3)
destroy(this.st_7)
destroy(this.st_4)
destroy(this.dw_1)
destroy(this.st_3)
destroy(this.em_orden)
destroy(this.cbx_camara)
end on

event open;call super::open;
dw_1.Object.tipo_transp[1] = 'M'

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

uo_clientes.dw_seleccion.enabled=true
uo_clientes.codigo = gi_codexport
uo_clientes.cbx_todos.checked=false
uo_clientes.cbx_todos.visible=false
uo_clientes.cbx_consolida.visible=false
uo_planta.dw_seleccion.enabled=true
uo_planta.codigo = gi_codplanta
uo_planta.cbx_todos.checked=false
uo_planta.cbx_todos.visible=false
uo_planta.cbx_consolida.visible=false

//uo_camara.codigo = gi_codplanta
//uo_camara.cbx_todos.checked=false
//uo_camara.cbx_todos.visible=false
//uo_camara.cbx_consolida.visible=false
dw_camara.settransobject(sqlca)
dw_camara.insertRow(0)

uo_clientes.dw_seleccion.object.codigo[1] = gi_codexport
uo_planta.dw_seleccion.object.codigo[1] = gi_codplanta

uo_clientes.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
uo_planta.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	


dw_camara.GetChild("cama_codigo", idwc_camara)

idwc_camara.SetTransObject(sqlca)

idwc_camara.Retrieve(gi_codplanta) 
end event

type st_computador from w_para_informes`st_computador within w_info_conspalletsordenembarque
end type

type st_usuario from w_para_informes`st_usuario within w_info_conspalletsordenembarque
end type

type st_temporada from w_para_informes`st_temporada within w_info_conspalletsordenembarque
end type

type p_logo from w_para_informes`p_logo within w_info_conspalletsordenembarque
end type

type st_titulo from w_para_informes`st_titulo within w_info_conspalletsordenembarque
integer width = 2743
string text = "Informe Consolidado de Carga Pallet"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_conspalletsordenembarque
integer x = 3150
integer y = 820
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
ELSEIF (IsNull(dw_camara.object.cama_codigo[1])  OR dw_camara.object.cama_codigo[1] = 0) and il_camara = 0 THEN
 MessageBox("Atención", "Debe Ingresar Cámara Previamente ",Exclamation!)
	RETURN
END IF

ls_Trans		=	dw_1.Object.tipo_transp[1]
li_cliente	=	uo_clientes.codigo
li_NroOrden	=	Long(em_orden.text)
li_Planta	=	uo_planta.codigo

Parent.TriggerEvent("ue_imprimir")
end event

type pb_salir from w_para_informes`pb_salir within w_info_conspalletsordenembarque
integer x = 3150
integer y = 1096
end type

type uo_planta from uo_seleccion_plantas within w_info_conspalletsordenembarque
integer x = 681
integer y = 728
integer width = 974
integer taborder = 30
boolean bringtotop = true
end type

on uo_planta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;integer li_nula

setnull(li_nula)

IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
//		uo_camara.Todos(True)
//		
//		uo_camara.cbx_Todos.Enabled	=	False
		
	CASE ELSE
//		uo_camara.Filtra(This.Codigo)
//		uo_camara.cbx_Todos.Enabled	=	True
dw_camara.SetItem(1, "cama_codigo", li_Nula)
idwc_camara.Retrieve(this.codigo)
END CHOOSE
end event

type uo_clientes from uo_seleccion_clientesprod within w_info_conspalletsordenembarque
integer x = 681
integer y = 540
integer width = 974
integer taborder = 40
boolean bringtotop = true
end type

on uo_clientes.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_1 from statictext within w_info_conspalletsordenembarque
integer x = 366
integer y = 644
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

type st_2 from statictext within w_info_conspalletsordenembarque
integer x = 366
integer y = 832
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

type cb_1 from commandbutton within w_info_conspalletsordenembarque
integer x = 2752
integer y = 808
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

type rb_general from radiobutton within w_info_conspalletsordenembarque
integer x = 681
integer y = 1244
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

type rb_clasificado from radiobutton within w_info_conspalletsordenembarque
integer x = 1111
integer y = 1244
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

type gb_4 from groupbox within w_info_conspalletsordenembarque
integer x = 306
integer y = 1168
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

type st_5 from statictext within w_info_conspalletsordenembarque
integer x = 366
integer y = 1256
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

type st_6 from statictext within w_info_conspalletsordenembarque
integer x = 366
integer y = 1020
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
string text = "Cámara"
boolean focusrectangle = false
end type

type dw_camara from datawindow within w_info_conspalletsordenembarque
integer x = 663
integer y = 972
integer width = 910
integer height = 120
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_camarasbode"
boolean border = false
boolean livescroll = true
end type

event itemchanged;integer li_count ,li_codigo,li_nula

setnull(li_nula)

li_codigo=integer(data)

select count(*) into :li_count
  from dba.camarasbode
 where plde_codigo=:uo_planta.codigo
   and cama_codigo=:li_codigo
 using sqlca;

if li_count=0 then
	This.SetItem(1, "cama_codigo", li_Nula)
	messagebox('Atención','Cámara no existe para la Planta Seleccionada')
   return 1
end if
 
end event

type gb_3 from groupbox within w_info_conspalletsordenembarque
integer x = 306
integer y = 480
integer width = 2619
integer height = 660
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_7 from statictext within w_info_conspalletsordenembarque
integer x = 251
integer y = 440
integer width = 2743
integer height = 988
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

type st_4 from statictext within w_info_conspalletsordenembarque
integer x = 1669
integer y = 636
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

type dw_1 from datawindow within w_info_conspalletsordenembarque
integer x = 2158
integer y = 616
integer width = 699
integer height = 132
integer taborder = 30
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

type st_3 from statictext within w_info_conspalletsordenembarque
integer x = 1669
integer y = 832
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

type em_orden from editmask within w_info_conspalletsordenembarque
integer x = 2176
integer y = 812
integer width = 439
integer height = 80
integer taborder = 40
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

type cbx_camara from checkbox within w_info_conspalletsordenembarque
integer x = 1586
integer y = 980
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Todos"
end type

event clicked;integer li_nula

setnull(li_nula)

if cbx_camara.checked = true then
   il_camara = -1 
	dw_camara.enabled=false
	//dw_camara.object.cama_codigo.BackGround.Color	=	RGB(166,180,210)
else
	il_camara = 0 
	dw_camara.enabled=true
//	dw_camara.object.cama_codigo.BackGround.Color	=	RGB(255, 255, 255)
end if
end event

