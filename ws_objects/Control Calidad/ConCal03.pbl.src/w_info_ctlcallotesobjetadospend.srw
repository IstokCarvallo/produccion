$PBExportHeader$w_info_ctlcallotesobjetadospend.srw
$PBExportComments$Ventana Informe de Planilla de Lotes Objetados Pendientes.
forward
global type w_info_ctlcallotesobjetadospend from w_para_informes
end type
type st_1 from statictext within w_info_ctlcallotesobjetadospend
end type
type dw_planta from datawindow within w_info_ctlcallotesobjetadospend
end type
type st_33 from statictext within w_info_ctlcallotesobjetadospend
end type
type rb_infogerencia from radiobutton within w_info_ctlcallotesobjetadospend
end type
type rb_infoplanta from radiobutton within w_info_ctlcallotesobjetadospend
end type
type gb_3 from groupbox within w_info_ctlcallotesobjetadospend
end type
type st_45 from statictext within w_info_ctlcallotesobjetadospend
end type
type cbx_fechaemba from checkbox within w_info_ctlcallotesobjetadospend
end type
type em_fechaemb from editmask within w_info_ctlcallotesobjetadospend
end type
type em_fechahasta from editmask within w_info_ctlcallotesobjetadospend
end type
type st_3 from statictext within w_info_ctlcallotesobjetadospend
end type
type st_4 from statictext within w_info_ctlcallotesobjetadospend
end type
type gb_4 from groupbox within w_info_ctlcallotesobjetadospend
end type
type st_44 from statictext within w_info_ctlcallotesobjetadospend
end type
type st_2 from statictext within w_info_ctlcallotesobjetadospend
end type
type dw_especie from datawindow within w_info_ctlcallotesobjetadospend
end type
type st_5 from statictext within w_info_ctlcallotesobjetadospend
end type
type cbx_todoszona from checkbox within w_info_ctlcallotesobjetadospend
end type
type cbx_todoplanta from checkbox within w_info_ctlcallotesobjetadospend
end type
type dw_zona from datawindow within w_info_ctlcallotesobjetadospend
end type
end forward

global type w_info_ctlcallotesobjetadospend from w_para_informes
integer x = 14
integer y = 32
integer width = 2670
integer height = 1720
string title = "Informe Pallet Objetados Pendientes"
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
global w_info_ctlcallotesobjetadospend w_info_ctlcallotesobjetadospend

type variables
str_busqueda istr_busq
str_mant istr_mant

Integer	ii_tipo, codigo
String		is_report, nombre

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_zona,idwc_especie


uo_especie				iuo_especie
uo_zonas				iuo_zonas
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

on w_info_ctlcallotesobjetadospend.create
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

on w_info_ctlcallotesobjetadospend.destroy
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

type pb_excel from w_para_informes`pb_excel within w_info_ctlcallotesobjetadospend
end type

type st_computador from w_para_informes`st_computador within w_info_ctlcallotesobjetadospend
integer x = 1701
end type

type st_usuario from w_para_informes`st_usuario within w_info_ctlcallotesobjetadospend
integer x = 1701
end type

type st_temporada from w_para_informes`st_temporada within w_info_ctlcallotesobjetadospend
integer x = 1701
end type

type p_logo from w_para_informes`p_logo within w_info_ctlcallotesobjetadospend
end type

type st_titulo from w_para_informes`st_titulo within w_info_ctlcallotesobjetadospend
integer width = 1934
string text = "Informe de Pallet Rechazados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_ctlcallotesobjetadospend
string tag = "Imprimir Reporte"
integer x = 2304
integer y = 564
integer taborder = 110
end type

event pb_acepta::clicked;Integer	li_fila, li_planta, li_cliente,li_zona,li_especie
Date 		ld_FechaEmbaIni, ld_FechaEmbaFin

istr_info.titulo	= 'INFORME PALLET OBJETADOS PEDIENTES'

OpenWithParm(vinf,istr_info)

If cbx_fechaemba.Checked = False Then
	ld_FechaEmbaIni	=	Date(em_fechaemb.Text)
	ld_FechaEmbaFin	=	Date(em_fechahasta.Text)
Else
	ld_fechaEmbaini   =	Date('01/01/1900')
	ld_FechaEmbaFin	=	Date(today())
End If

If cbx_todoszona.Checked Then
	  li_zona	= -1
Else
  li_zona	= dw_zona.Object.zona_codigo[1]
  If IsNull(li_zona)Then
	  MessageBox("Atención","Debe Seleccionar una Zona Previamente",Exclamation!)
	  Return
  End If
End If

If cbx_todoplanta.Checked Then
    li_planta	 = -1
Else
	li_planta	 = dw_planta.Object.plde_codigo[1]
	If IsNull(li_planta)Then
		MessageBox("Atención","Debe Seleccionar una Planta Previamente",Exclamation!)
		Return
	End If
End If

li_especie	= dw_especie.Object.espe_codigo[1]

If ld_FechaEmbaIni > ld_FechaEmbaFin Then
	MessageBox("Atención", "Fecha Final debe ser igual o posterior~r" + &
					"a Fecha Inicial~r~rIngrese otras fechas.")	
	em_fechaemb.SetFocus()	
	Return
End If

If rb_infogerencia.Checked	Then
	vinf.dw_1.DataObject = "dw_info_objetados_pendiente_gerencia"	
	vinf.dw_1.SetTransObject(sqlca)
ElseIf rb_infoplanta.Checked AND li_especie = 11 Then
	vinf.dw_1.DataObject = "dw_info_ctlcallotesobjetadospend_planta"
	vinf.dw_1.SetTransObject(sqlca)
Else	
	vinf.dw_1.DataObject = "dw_info_ctlcallotesobjetadospend_esp"	
	vinf.dw_1.SetTransObject(sqlca)
End If

li_fila	=	vinf.dw_1.Retrieve(li_planta, li_zona, ld_FechaEmbaIni, ld_FechaEmbaFin, li_especie)
									  
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No hay Lotes Objetados.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("desde.text = '" + em_fechaemb.text + "'")
	vinf.dw_1.ModIfy("hasta.text = '" + em_fechahasta.text + "'")
	vinf.dw_1.ModIfy('DataWindow.Zoom = 80')
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_ctlcallotesobjetadospend
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2304
integer y = 844
integer taborder = 120
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_ctlcallotesobjetadospend
integer x = 352
integer y = 800
integer width = 311
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
string text = "Frigorífico"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_ctlcallotesobjetadospend
integer x = 827
integer y = 788
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

type st_33 from statictext within w_info_ctlcallotesobjetadospend
integer x = 357
integer y = 664
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

type rb_infogerencia from radiobutton within w_info_ctlcallotesobjetadospend
integer x = 553
integer y = 1236
integer width = 590
integer height = 80
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Informe Gerencia"
end type

type rb_infoplanta from radiobutton within w_info_ctlcallotesobjetadospend
integer x = 1280
integer y = 1236
integer width = 512
integer height = 80
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Informe Planta"
boolean checked = true
end type

type gb_3 from groupbox within w_info_ctlcallotesobjetadospend
integer x = 297
integer y = 1176
integer width = 1838
integer height = 168
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
end type

type st_45 from statictext within w_info_ctlcallotesobjetadospend
integer x = 251
integer y = 1168
integer width = 1934
integer height = 220
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

type cbx_fechaemba from checkbox within w_info_ctlcallotesobjetadospend
integer x = 1870
integer y = 1012
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

type em_fechaemb from editmask within w_info_ctlcallotesobjetadospend
integer x = 613
integer y = 1000
integer width = 480
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 33543637
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type em_fechahasta from editmask within w_info_ctlcallotesobjetadospend
integer x = 1321
integer y = 1000
integer width = 480
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_3 from statictext within w_info_ctlcallotesobjetadospend
integer x = 352
integer y = 1008
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

type st_4 from statictext within w_info_ctlcallotesobjetadospend
integer x = 1120
integer y = 1008
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

type gb_4 from groupbox within w_info_ctlcallotesobjetadospend
integer x = 320
integer y = 920
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

type st_44 from statictext within w_info_ctlcallotesobjetadospend
integer x = 251
integer y = 440
integer width = 1934
integer height = 724
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

type st_2 from statictext within w_info_ctlcallotesobjetadospend
integer x = 352
integer y = 528
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

type dw_especie from datawindow within w_info_ctlcallotesobjetadospend
integer x = 832
integer y = 524
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

type st_5 from statictext within w_info_ctlcallotesobjetadospend
integer x = 1801
integer y = 472
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
boolean focusrectangle = false
end type

type cbx_todoszona from checkbox within w_info_ctlcallotesobjetadospend
integer x = 1865
integer y = 652
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

type cbx_todoplanta from checkbox within w_info_ctlcallotesobjetadospend
integer x = 1865
integer y = 792
integer width = 119
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

type dw_zona from datawindow within w_info_ctlcallotesobjetadospend
integer x = 832
integer y = 660
integer width = 827
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

