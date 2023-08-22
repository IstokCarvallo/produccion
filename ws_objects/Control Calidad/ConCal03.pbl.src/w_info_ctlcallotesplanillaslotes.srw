$PBExportHeader$w_info_ctlcallotesplanillaslotes.srw
$PBExportComments$Ventana Informe de Planilla de Lotes Objetados Pendientes.
forward
global type w_info_ctlcallotesplanillaslotes from w_para_informes
end type
type st_1 from statictext within w_info_ctlcallotesplanillaslotes
end type
type dw_planta from datawindow within w_info_ctlcallotesplanillaslotes
end type
type st_33 from statictext within w_info_ctlcallotesplanillaslotes
end type
type rb_infogerencia from radiobutton within w_info_ctlcallotesplanillaslotes
end type
type rb_infoplanta from radiobutton within w_info_ctlcallotesplanillaslotes
end type
type gb_3 from groupbox within w_info_ctlcallotesplanillaslotes
end type
type st_45 from statictext within w_info_ctlcallotesplanillaslotes
end type
type cbx_fechaemba from checkbox within w_info_ctlcallotesplanillaslotes
end type
type em_fechaemb from editmask within w_info_ctlcallotesplanillaslotes
end type
type em_fechahasta from editmask within w_info_ctlcallotesplanillaslotes
end type
type st_3 from statictext within w_info_ctlcallotesplanillaslotes
end type
type st_4 from statictext within w_info_ctlcallotesplanillaslotes
end type
type gb_4 from groupbox within w_info_ctlcallotesplanillaslotes
end type
type st_44 from statictext within w_info_ctlcallotesplanillaslotes
end type
type st_2 from statictext within w_info_ctlcallotesplanillaslotes
end type
type dw_especie from datawindow within w_info_ctlcallotesplanillaslotes
end type
type st_5 from statictext within w_info_ctlcallotesplanillaslotes
end type
type cbx_todoszona from checkbox within w_info_ctlcallotesplanillaslotes
end type
type cbx_todoplanta from checkbox within w_info_ctlcallotesplanillaslotes
end type
type dw_zona from datawindow within w_info_ctlcallotesplanillaslotes
end type
end forward

global type w_info_ctlcallotesplanillaslotes from w_para_informes
integer x = 14
integer y = 32
integer width = 2688
integer height = 1272
string title = "Informe Lotes Objetados Pendientes"
string icon = "F:\Desarrollo\Productiva\Control Calidad\ConCal.ico"
st_1 st_1
dw_planta dw_planta
st_33 st_33
rb_infogerencia rb_infogerencia
rb_infoplanta rb_infoplanta
gb_3 gb_3
st_45 st_45
cbx_fechaemba cbx_fechaemba
em_fechaemb em_fechaemb
em_fechahasta em_fechahasta
st_3 st_3
st_4 st_4
gb_4 gb_4
st_44 st_44
st_2 st_2
dw_especie dw_especie
st_5 st_5
cbx_todoszona cbx_todoszona
cbx_todoplanta cbx_todoplanta
dw_zona dw_zona
end type
global w_info_ctlcallotesplanillaslotes w_info_ctlcallotesplanillaslotes

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo, codigo
String	is_report, nombre

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_zona,idwc_especie


uo_especie				iuo_especie
uo_zonas					iuo_zonas
end variables

forward prototypes
public function boolean noexisteplanta (integer planta, integer tipo)
end prototypes

public function boolean noexisteplanta (integer planta, integer tipo);Integer li_Contador, li_cliente,li_zona

li_cliente	=	gi_CodExport

Select Count(*)
Into :li_Contador
From dbo.plantadesp
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
		messagebox("Atención","Código Packing No Existe Para la Zona" + &
						"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	END IF					
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
	
end function

on w_info_ctlcallotesplanillaslotes.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_planta=create dw_planta
this.st_33=create st_33
this.rb_infogerencia=create rb_infogerencia
this.rb_infoplanta=create rb_infoplanta
this.gb_3=create gb_3
this.st_45=create st_45
this.cbx_fechaemba=create cbx_fechaemba
this.em_fechaemb=create em_fechaemb
this.em_fechahasta=create em_fechahasta
this.st_3=create st_3
this.st_4=create st_4
this.gb_4=create gb_4
this.st_44=create st_44
this.st_2=create st_2
this.dw_especie=create dw_especie
this.st_5=create st_5
this.cbx_todoszona=create cbx_todoszona
this.cbx_todoplanta=create cbx_todoplanta
this.dw_zona=create dw_zona
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_planta
this.Control[iCurrent+3]=this.st_33
this.Control[iCurrent+4]=this.rb_infogerencia
this.Control[iCurrent+5]=this.rb_infoplanta
this.Control[iCurrent+6]=this.gb_3
this.Control[iCurrent+7]=this.st_45
this.Control[iCurrent+8]=this.cbx_fechaemba
this.Control[iCurrent+9]=this.em_fechaemb
this.Control[iCurrent+10]=this.em_fechahasta
this.Control[iCurrent+11]=this.st_3
this.Control[iCurrent+12]=this.st_4
this.Control[iCurrent+13]=this.gb_4
this.Control[iCurrent+14]=this.st_44
this.Control[iCurrent+15]=this.st_2
this.Control[iCurrent+16]=this.dw_especie
this.Control[iCurrent+17]=this.st_5
this.Control[iCurrent+18]=this.cbx_todoszona
this.Control[iCurrent+19]=this.cbx_todoplanta
this.Control[iCurrent+20]=this.dw_zona
end on

on w_info_ctlcallotesplanillaslotes.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_planta)
destroy(this.st_33)
destroy(this.rb_infogerencia)
destroy(this.rb_infoplanta)
destroy(this.gb_3)
destroy(this.st_45)
destroy(this.cbx_fechaemba)
destroy(this.em_fechaemb)
destroy(this.em_fechahasta)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.gb_4)
destroy(this.st_44)
destroy(this.st_2)
destroy(this.dw_especie)
destroy(this.st_5)
destroy(this.cbx_todoszona)
destroy(this.cbx_todoplanta)
destroy(this.dw_zona)
end on

event open;call super::open;
x=0
y=0

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
idwc_planta.SetSort("plde_nombre A")
idwc_planta.Sort()

dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()
dw_zona.InsertRow(0)
idwc_zona.SetSort("zona_nombre A")
idwc_zona.Sort()

//Especie
dw_Especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_Especie.InsertRow(1)
idwc_zona.SetSort("espe_nombre A")
idwc_zona.Sort()
dw_Especie.SetItem(1,"espe_codigo",11)

iuo_especie  			=	Create uo_especie
iuo_zonas				=  Create uo_zonas

em_fechaemb.Text			=	String(RelativeDate ( Today(), -365 ))
em_fechahasta.Text		=	String(Today())
end event

type pb_excel from w_para_informes`pb_excel within w_info_ctlcallotesplanillaslotes
end type

type st_computador from w_para_informes`st_computador within w_info_ctlcallotesplanillaslotes
end type

type st_usuario from w_para_informes`st_usuario within w_info_ctlcallotesplanillaslotes
end type

type st_temporada from w_para_informes`st_temporada within w_info_ctlcallotesplanillaslotes
end type

type p_logo from w_para_informes`p_logo within w_info_ctlcallotesplanillaslotes
end type

type st_titulo from w_para_informes`st_titulo within w_info_ctlcallotesplanillaslotes
integer width = 1934
string text = "Informe de Planillas Modificadas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_ctlcallotesplanillaslotes
string tag = "Imprimir Reporte"
integer x = 2322
integer y = 528
integer taborder = 110
end type

event pb_acepta::clicked;Integer	li_fila, li_planta, li_cliente,li_zona,li_especie
Date 		ld_FechaEmbaIni, ld_FechaEmbaFin

SetPointer(Arrow!)

istr_info.titulo	= 'INFORME PLANILLAS MODIFICADAS'

OpenWithParm(vinf,istr_info)

IF cbx_fechaemba.Checked = False THEN
	ld_FechaEmbaIni	=	Date(em_fechaemb.Text)
	ld_FechaEmbaFin	=	Date(em_fechahasta.Text)
ELSE
	ld_fechaEmbaini   =	Date('01/01/1900')
	ld_FechaEmbaFin	=	Date(today())
END IF

IF cbx_todoplanta.Checked THEN
    li_planta	 = -1
ELSE
	li_planta	 = dw_planta.Object.plde_codigo[1]
	IF IsNull(li_planta)THEN
		MessageBox("Atención","Debe Seleccionar una Planta Previamente",Exclamation!)
		RETURN
	END IF
END IF

li_especie	= dw_especie.Object.espe_codigo[1]

IF ld_FechaEmbaIni > ld_FechaEmbaFin THEN
	MessageBox("Atención", "Fecha Final debe ser igual o posterior~r" + &
					"a Fecha Inicial~r~rIngrese otras fechas.")	
	em_fechaemb.SetFocus()	
	RETURN
END IF

vinf.dw_1.DataObject = "dw_info_ctlcallotesplanillaslotes"
vinf.dw_1.SetTransObject(sqlca)

li_fila	=	vinf.dw_1.Retrieve(li_planta, li_especie, ld_FechaEmbaIni, ld_FechaEmbaFin)
									  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No hay Lotes Objetados.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("desde.text = '" + em_fechaemb.text + "'")
	vinf.dw_1.Modify("hasta.text = '" + em_fechahasta.text + "'")
	vinf.dw_1.Modify('DataWindow.Zoom = 83')
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
		

END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_ctlcallotesplanillaslotes
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2322
integer y = 808
integer taborder = 120
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_ctlcallotesplanillaslotes
integer x = 325
integer y = 632
integer width = 311
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_ctlcallotesplanillaslotes
integer x = 805
integer y = 620
integer width = 960
integer height = 96
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_planta.PostEvent(Clicked!)

IF NoExistePlanta(Integer(data),1) THEN
	This.SetItem(1, "plde_codigo", Long(ls_nula))
	RETURN 1
END IF	
end event

event itemerror;Return 1
end event

event clicked;cbx_todoplanta.Checked = False
end event

type st_33 from statictext within w_info_ctlcallotesplanillaslotes
boolean visible = false
integer x = 329
integer y = 632
integer width = 480
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
string text = "Zona Origen"
boolean focusrectangle = false
end type

type rb_infogerencia from radiobutton within w_info_ctlcallotesplanillaslotes
boolean visible = false
integer x = 571
integer y = 1252
integer width = 590
integer height = 80
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Informe Gerencia"
end type

type rb_infoplanta from radiobutton within w_info_ctlcallotesplanillaslotes
boolean visible = false
integer x = 1298
integer y = 1252
integer width = 512
integer height = 80
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Informe Planta"
boolean checked = true
end type

type gb_3 from groupbox within w_info_ctlcallotesplanillaslotes
boolean visible = false
integer x = 315
integer y = 1192
integer width = 1838
integer height = 168
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_45 from statictext within w_info_ctlcallotesplanillaslotes
boolean visible = false
integer x = 270
integer y = 1184
integer width = 1934
integer height = 220
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

type cbx_fechaemba from checkbox within w_info_ctlcallotesplanillaslotes
integer x = 1842
integer y = 832
integer width = 128
integer height = 64
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
string text = " "
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_fechaemb.Enabled		=	False
	em_fechahasta.Enabled	=	False
	em_fechaemb.Text			=	'01/01/1900'
	em_fechahasta.Text		=	"31/12/2199"
ELSE
	em_fechaemb.Enabled	   =	True
	em_fechahasta.Enabled	=	True
	em_fechaemb.SetFocus()
END IF

RETURN 0
end event

type em_fechaemb from editmask within w_info_ctlcallotesplanillaslotes
integer x = 686
integer y = 820
integer width = 375
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_fechahasta from editmask within w_info_ctlcallotesplanillaslotes
integer x = 1294
integer y = 820
integer width = 375
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_3 from statictext within w_info_ctlcallotesplanillaslotes
integer x = 325
integer y = 828
integer width = 210
integer height = 76
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
string text = "Desde"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_ctlcallotesplanillaslotes
integer x = 1093
integer y = 828
integer width = 183
integer height = 76
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
string text = "Hasta"
boolean focusrectangle = false
end type

type gb_4 from groupbox within w_info_ctlcallotesplanillaslotes
integer x = 293
integer y = 740
integer width = 1838
integer height = 228
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Embalaje"
end type

type st_44 from statictext within w_info_ctlcallotesplanillaslotes
integer x = 247
integer y = 420
integer width = 1934
integer height = 608
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_ctlcallotesplanillaslotes
integer x = 325
integer y = 488
integer width = 247
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
string text = "Especie"
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_ctlcallotesplanillaslotes
integer x = 805
integer y = 484
integer width = 859
integer height = 92
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Nula

SetNull(li_Nula)
IF iuo_Especie.Existe(Integer(data), True, sqlca) =False THEN
		This.SetItem(1, "espe_codigo", long(li_Nula))
		RETURN 1
END IF


end event

event itemerror;RETURN 1
end event

type st_5 from statictext within w_info_ctlcallotesplanillaslotes
integer x = 1774
integer y = 432
integer width = 247
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
string text = "Todos"
alignment alignment = center!
boolean focusrectangle = false
end type

type cbx_todoszona from checkbox within w_info_ctlcallotesplanillaslotes
integer x = 1838
integer y = 612
integer width = 101
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
string text = " "
boolean checked = true
end type

event clicked;Integer	li_null

SetNull(li_null)
IF This.Checked THEN	
	dw_zona.SetItem(1, "zona_codigo", li_null)
	/*Planta*/
	idwc_planta.Retrieve(gi_codexport,1,0)
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
	
ELSE
	dw_zona.Setfocus()
	dw_zona.SetItem(1, "zona_codigo", li_null)
	/*Planta*/
	idwc_planta.Retrieve(gi_codexport,1,gi_codzona)
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
END IF
RETURN 0



end event

type cbx_todoplanta from checkbox within w_info_ctlcallotesplanillaslotes
integer x = 1838
integer y = 624
integer width = 91
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " "
boolean checked = true
end type

event clicked;Integer	li_null

SetNull(li_Null)

IF This.Checked THEN	
   dw_planta.SetItem(1, "plde_codigo", li_null)
ELSE
	dw_planta.Setfocus()
	dw_planta.SetItem(1, "plde_codigo", li_null)
	
END IF
RETURN 0

end event

type dw_zona from datawindow within w_info_ctlcallotesplanillaslotes
boolean visible = false
integer x = 805
integer y = 620
integer width = 846
integer height = 92
integer taborder = 30
boolean bringtotop = true
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_zona.PostEvent(Clicked!)

IF iuo_zonas.existe(Integer(Data),True,sqlca) = False THEN
	This.SetItem(1, "zona_codigo", Long(ls_nula))
	RETURN 1
	
ELSE
	RETURN 0
END IF	


end event

event itemerror;Return 1
end event

event clicked;cbx_todoszona.Checked = False
end event

