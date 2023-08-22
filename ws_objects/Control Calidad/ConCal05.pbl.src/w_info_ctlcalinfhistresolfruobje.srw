$PBExportHeader$w_info_ctlcalinfhistresolfruobje.srw
$PBExportComments$Ventana Informe de Histórico de Inspecciónes.
forward
global type w_info_ctlcalinfhistresolfruobje from w_para_informes
end type
type st_4 from statictext within w_info_ctlcalinfhistresolfruobje
end type
type st_1 from statictext within w_info_ctlcalinfhistresolfruobje
end type
type dw_1 from datawindow within w_info_ctlcalinfhistresolfruobje
end type
type em_fechadesde from editmask within w_info_ctlcalinfhistresolfruobje
end type
type st_33 from statictext within w_info_ctlcalinfhistresolfruobje
end type
type dw_11 from datawindow within w_info_ctlcalinfhistresolfruobje
end type
type st_44 from statictext within w_info_ctlcalinfhistresolfruobje
end type
type st_12 from statictext within w_info_ctlcalinfhistresolfruobje
end type
type st_13 from statictext within w_info_ctlcalinfhistresolfruobje
end type
type em_fechahasta from editmask within w_info_ctlcalinfhistresolfruobje
end type
type cbx_1 from checkbox within w_info_ctlcalinfhistresolfruobje
end type
type st_14 from statictext within w_info_ctlcalinfhistresolfruobje
end type
end forward

global type w_info_ctlcalinfhistresolfruobje from w_para_informes
integer x = 14
integer y = 32
integer width = 2473
integer height = 1068
string title = "Histórico de Resoluciones de Fruta Objetada"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Productiva\Control Calidad\ConCal.ico"
st_4 st_4
st_1 st_1
dw_1 dw_1
em_fechadesde em_fechadesde
st_33 st_33
dw_11 dw_11
st_44 st_44
st_12 st_12
st_13 st_13
em_fechahasta em_fechahasta
cbx_1 cbx_1
st_14 st_14
end type
global w_info_ctlcalinfhistresolfruobje w_info_ctlcalinfhistresolfruobje

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo
String	is_report

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_zona, idwc_productores
end variables

forward prototypes
public function boolean noexisteplanta (integer planta, integer tipo)
public function boolean noexisteproductor (integer al_productor)
end prototypes

public function boolean noexisteplanta (integer planta, integer tipo);Integer li_Contador, li_cliente,li_zona

li_cliente	=	gi_CodExport

Select Count(*)
Into :li_Contador
From dba.plantadesp
Where plde_codigo = :planta
and	plde_tipopl	= :Tipo;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Plantas Despachos")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	IF Tipo = 1 THEN
		messagebox("Atención","Código Planta No Existe Para la Zona" + &
						"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	ELSE		
		messagebox("Atención","Código Frigorífico No Existe Para la Zona" + &
						"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	END IF					
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
	
end function

public function boolean noexisteproductor (integer al_productor);Integer li_Contador, li_cliente

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.productores
	WHERE	:al_productor in(0,prod_codigo);
	

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Productor")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Productor No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

on w_info_ctlcalinfhistresolfruobje.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.dw_1=create dw_1
this.em_fechadesde=create em_fechadesde
this.st_33=create st_33
this.dw_11=create dw_11
this.st_44=create st_44
this.st_12=create st_12
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.cbx_1=create cbx_1
this.st_14=create st_14
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_1
this.Control[iCurrent+4]=this.em_fechadesde
this.Control[iCurrent+5]=this.st_33
this.Control[iCurrent+6]=this.dw_11
this.Control[iCurrent+7]=this.st_44
this.Control[iCurrent+8]=this.st_12
this.Control[iCurrent+9]=this.st_13
this.Control[iCurrent+10]=this.em_fechahasta
this.Control[iCurrent+11]=this.cbx_1
this.Control[iCurrent+12]=this.st_14
end on

on w_info_ctlcalinfhistresolfruobje.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.dw_1)
destroy(this.em_fechadesde)
destroy(this.st_33)
destroy(this.dw_11)
destroy(this.st_44)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.cbx_1)
destroy(this.st_14)
end on

event open;/***********************************

Argumento[1] = Código Cliente
Argumento[2] = Código Planta
Argumento[3] = Productor
Argumento[4] = Fecha Desde
Argumento[5] = Fecha Hasta

***********************************/

x=0
y=0

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
idwc_planta.SetSort("plde_nombre A")
idwc_planta.Sort()
dw_1.InsertRow(0)
dw_1.SetItem(1,"plde_codigo", gi_codplanta)

dw_11.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve()
idwc_productores.SetSort("prod_nombre A")
idwc_productores.Sort()
dw_11.InsertRow(0)

istr_mant.argumento[1] 	=	String(gi_codexport)
istr_mant.argumento[2] 	=	String(gi_codplanta)
istr_mant.argumento[3] 	=	"0"
istr_mant.argumento[4] 	=	String(Today())
istr_mant.argumento[5] 	=	String(Today())
em_fechadesde.text		=	String(Today())
em_fechahasta.text		=	String(Today())
end event

type st_computador from w_para_informes`st_computador within w_info_ctlcalinfhistresolfruobje
end type

type st_usuario from w_para_informes`st_usuario within w_info_ctlcalinfhistresolfruobje
end type

type st_temporada from w_para_informes`st_temporada within w_info_ctlcalinfhistresolfruobje
end type

type p_logo from w_para_informes`p_logo within w_info_ctlcalinfhistresolfruobje
end type

type st_titulo from w_para_informes`st_titulo within w_info_ctlcalinfhistresolfruobje
integer x = 91
integer y = 64
integer width = 1934
string text = "Histórico de Resoluciones de Fruta Objetada"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_ctlcalinfhistresolfruobje
string tag = "Imprimir Reporte"
integer x = 2176
integer y = 328
integer taborder = 60
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	li_fila, li_planta, li_cliente
Long		ll_planilla_sag

istr_info.titulo	= 'HISTORICO DE RESOLUCIONES DE FRUTA OBJETADA'

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_ctlcalinfhistresolfruobjetadas"

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]), &
                             Long(istr_mant.argumento[3]),date(istr_mant.argumento[4]),date(istr_mant.argumento[5]))
								  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("desde.text = '" + em_fechadesde.text + "'")
	vinf.dw_1.Modify("hasta.text = '" + em_fechahasta.text + "'")	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_ctlcalinfhistresolfruobje
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2171
integer y = 608
integer taborder = 70
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_ctlcalinfhistresolfruobje
integer x = 91
integer y = 632
integer width = 1934
integer height = 288
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_ctlcalinfhistresolfruobje
integer x = 128
integer y = 312
integer width = 311
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Frigorífico"
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_info_ctlcalinfhistresolfruobje
integer x = 489
integer y = 284
integer width = 960
integer height = 92
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

IF NoExistePlanta(Integer(data),1) THEN
	This.SetItem(1, "plde_codigo", Long(ls_nula))
	RETURN 1
END IF	


istr_mant.argumento[2] = String(data)
end event

event itemerror;Return 1
end event

type em_fechadesde from editmask within w_info_ctlcalinfhistresolfruobje
integer x = 530
integer y = 776
integer width = 402
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_Mant.Argumento[4] = This.Text
end event

type st_33 from statictext within w_info_ctlcalinfhistresolfruobje
integer x = 128
integer y = 444
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type dw_11 from datawindow within w_info_ctlcalinfhistresolfruobje
integer x = 489
integer y = 436
integer width = 1216
integer height = 96
integer taborder = 30
boolean bringtotop = true
string dataobject = "dddw_productores_cliente"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[3] = String(data)

String ls_Nula

SetNull(ls_Nula)
 IF NoExisteProductor(Long(data)) THEN
    This.SetItem(1,"prod_codigo", Long(ls_nula))
    RETURN 1
ELSE
	//idwc_tecnico.Retrieve(Long(data),Integer(dw_zona.object.zona_codigo[1]))
END IF	


dw_11.PostEvent(Clicked!)
end event

event clicked;cbx_1.Checked = False
end event

event itemerror;Return 1
end event

type st_44 from statictext within w_info_ctlcalinfhistresolfruobje
integer x = 91
integer y = 196
integer width = 1934
integer height = 436
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_ctlcalinfhistresolfruobje
integer x = 169
integer y = 776
integer width = 265
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Desde el"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_ctlcalinfhistresolfruobje
integer x = 1051
integer y = 776
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Hasta el"
boolean focusrectangle = false
end type

type em_fechahasta from editmask within w_info_ctlcalinfhistresolfruobje
integer x = 1376
integer y = 776
integer width = 402
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_Mant.Argumento[5] = This.Text
end event

type cbx_1 from checkbox within w_info_ctlcalinfhistresolfruobje
integer x = 1719
integer y = 444
integer width = 297
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;integer li_null
Setnull(li_null)
IF This.CheCked THEN
	//dw_11.Enabled = False
	//dw_11.SetTabOrder("prod_codigo",0)
	//dw_11.Modify("prod_codigo.BackGround.Color = " + String(rgb(192,192,192)))
   istr_mant.argumento[3]	=	'0'
	dw_11.SetItem(1, "prod_codigo", Long(li_null))
ELSE
	dw_11.SetFocus()
	//dw_11.Enabled = True
	//dw_11.SetTabOrder("prod_codigo", 20)
	//dw_11.Modify("prod_codigo.BackGround.Color = " + String(rgb(255,255,255)))
END IF
end event

type st_14 from statictext within w_info_ctlcalinfhistresolfruobje
integer x = 169
integer y = 672
integer width = 343
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Embalado"
boolean focusrectangle = false
end type

