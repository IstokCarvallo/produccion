$PBExportHeader$w_info_archivorepalletizado_generado.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_archivorepalletizado_generado from w_para_informes
end type
type dw_cliente from datawindow within w_info_archivorepalletizado_generado
end type
type st_2 from statictext within w_info_archivorepalletizado_generado
end type
type em_fdesde from editmask within w_info_archivorepalletizado_generado
end type
type st_8 from statictext within w_info_archivorepalletizado_generado
end type
type dw_planta from datawindow within w_info_archivorepalletizado_generado
end type
type em_fhasta from editmask within w_info_archivorepalletizado_generado
end type
type st_14 from statictext within w_info_archivorepalletizado_generado
end type
type st_5 from statictext within w_info_archivorepalletizado_generado
end type
type st_9 from statictext within w_info_archivorepalletizado_generado
end type
type em_ndesde from editmask within w_info_archivorepalletizado_generado
end type
type em_nhasta from editmask within w_info_archivorepalletizado_generado
end type
type gb_3 from groupbox within w_info_archivorepalletizado_generado
end type
type gb_4 from groupbox within w_info_archivorepalletizado_generado
end type
type st_12 from statictext within w_info_archivorepalletizado_generado
end type
type cbx_cliente from checkbox within w_info_archivorepalletizado_generado
end type
type cbx_planta from checkbox within w_info_archivorepalletizado_generado
end type
type cbx_1 from checkbox within w_info_archivorepalletizado_generado
end type
type cbx_2 from checkbox within w_info_archivorepalletizado_generado
end type
type gb_6 from groupbox within w_info_archivorepalletizado_generado
end type
type st_1 from statictext within w_info_archivorepalletizado_generado
end type
type rb_1 from radiobutton within w_info_archivorepalletizado_generado
end type
type rb_2 from radiobutton within w_info_archivorepalletizado_generado
end type
type rb_3 from radiobutton within w_info_archivorepalletizado_generado
end type
type cbx_3 from checkbox within w_info_archivorepalletizado_generado
end type
end forward

global type w_info_archivorepalletizado_generado from w_para_informes
integer width = 2907
integer height = 1728
string title = "ARCHIVOS GENERADOS"
boolean maxbox = false
boolean resizable = false
dw_cliente dw_cliente
st_2 st_2
em_fdesde em_fdesde
st_8 st_8
dw_planta dw_planta
em_fhasta em_fhasta
st_14 st_14
st_5 st_5
st_9 st_9
em_ndesde em_ndesde
em_nhasta em_nhasta
gb_3 gb_3
gb_4 gb_4
st_12 st_12
cbx_cliente cbx_cliente
cbx_planta cbx_planta
cbx_1 cbx_1
cbx_2 cbx_2
gb_6 gb_6
st_1 st_1
rb_1 rb_1
rb_2 rb_2
rb_3 rb_3
cbx_3 cbx_3
end type
global w_info_archivorepalletizado_generado w_info_archivorepalletizado_generado

type variables
str_mant istr_mant
DataWindowChild 		idwc_cliente, idwc_planta, idwc_tipop,idwc_mercados,&
                     idwc_productor, idwc_destino,idwc_pesoneto,idwc_embalaje							

Integer	ii_Cliente, ii_Planta, ii_filtro,ii_tipo,ii_destinos,ii_tipoi,ii_mercado
String	is_NomEspecie, is_NomPlanta, is_NomCliente, is_embalajes
Long		il_productor
Date		id_FechaZarpe

uo_productores     		iuo_productores   
uo_tipoproductor   		iuo_tipoproductor
uo_destinos        		iuo_destinos
uo_embalajesprod   		iuo_embalajesprod
uo_seleccion_especie		iuo_selespecie


end variables

on w_info_archivorepalletizado_generado.create
int iCurrent
call super::create
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.em_fdesde=create em_fdesde
this.st_8=create st_8
this.dw_planta=create dw_planta
this.em_fhasta=create em_fhasta
this.st_14=create st_14
this.st_5=create st_5
this.st_9=create st_9
this.em_ndesde=create em_ndesde
this.em_nhasta=create em_nhasta
this.gb_3=create gb_3
this.gb_4=create gb_4
this.st_12=create st_12
this.cbx_cliente=create cbx_cliente
this.cbx_planta=create cbx_planta
this.cbx_1=create cbx_1
this.cbx_2=create cbx_2
this.gb_6=create gb_6
this.st_1=create st_1
this.rb_1=create rb_1
this.rb_2=create rb_2
this.rb_3=create rb_3
this.cbx_3=create cbx_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cliente
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_fdesde
this.Control[iCurrent+4]=this.st_8
this.Control[iCurrent+5]=this.dw_planta
this.Control[iCurrent+6]=this.em_fhasta
this.Control[iCurrent+7]=this.st_14
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.st_9
this.Control[iCurrent+10]=this.em_ndesde
this.Control[iCurrent+11]=this.em_nhasta
this.Control[iCurrent+12]=this.gb_3
this.Control[iCurrent+13]=this.gb_4
this.Control[iCurrent+14]=this.st_12
this.Control[iCurrent+15]=this.cbx_cliente
this.Control[iCurrent+16]=this.cbx_planta
this.Control[iCurrent+17]=this.cbx_1
this.Control[iCurrent+18]=this.cbx_2
this.Control[iCurrent+19]=this.gb_6
this.Control[iCurrent+20]=this.st_1
this.Control[iCurrent+21]=this.rb_1
this.Control[iCurrent+22]=this.rb_2
this.Control[iCurrent+23]=this.rb_3
this.Control[iCurrent+24]=this.cbx_3
end on

on w_info_archivorepalletizado_generado.destroy
call super::destroy
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.em_fdesde)
destroy(this.st_8)
destroy(this.dw_planta)
destroy(this.em_fhasta)
destroy(this.st_14)
destroy(this.st_5)
destroy(this.st_9)
destroy(this.em_ndesde)
destroy(this.em_nhasta)
destroy(this.gb_3)
destroy(this.gb_4)
destroy(this.st_12)
destroy(this.cbx_cliente)
destroy(this.cbx_planta)
destroy(this.cbx_1)
destroy(this.cbx_2)
destroy(this.gb_6)
destroy(this.st_1)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.rb_3)
destroy(this.cbx_3)
end on

event open;call super::open;x				= 0
y				= 0
//This.Height	= 2020

String	ls_Columna[], ls_operacion="TODAS"
Integer 	li_busca
Boolean	lb_Cerrar

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.Object.clie_codigo[1]	=	gi_CodExport
li_busca = idwc_cliente.Find("clie_codigo = " + String(gi_CodExport), 1, idwc_cliente.RowCount())
is_NomCliente = idwc_cliente.GetItemString(li_busca, "clie_nombre")

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.Object.plde_codigo[1] = gi_CodPlanta
li_busca = idwc_planta.Find("plde_codigo = " + String(gi_CodPlanta), 1, idwc_planta.RowCount())
is_NomPlanta = idwc_planta.GetItemString(li_busca, "plde_nombre")

ii_Cliente						=	-1
ii_Planta						=	-1
istr_mant.argumento[1]		= 	String(gi_codexport)		//	Cliente
istr_mant.argumento[3]		= 	String(gi_CodPlanta)		//	Planta

em_fdesde.text					=	String(RelativeDate(Today() , -365))
em_fhasta.text					=	String(Today())	

em_ndesde.Text = '1'
em_nhasta.Text = '99999999'


end event

type pb_excel from w_para_informes`pb_excel within w_info_archivorepalletizado_generado
end type

type st_computador from w_para_informes`st_computador within w_info_archivorepalletizado_generado
end type

type st_usuario from w_para_informes`st_usuario within w_info_archivorepalletizado_generado
end type

type st_temporada from w_para_informes`st_temporada within w_info_archivorepalletizado_generado
end type

type p_logo from w_para_informes`p_logo within w_info_archivorepalletizado_generado
end type

type st_titulo from w_para_informes`st_titulo within w_info_archivorepalletizado_generado
integer width = 2171
string text = "Control Repalletizados Inspeccionados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_archivorepalletizado_generado
integer x = 2542
integer y = 896
integer taborder = 160
end type

event pb_acepta::clicked;Long	  	ll_Fila, ll_ndesde, ll_nhasta
Date		ld_fdesde, ld_fhasta
Integer	li_cliente, li_planta, li_filtra, li_sag

SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME CONTROL REPALLETIZADOS INSPECCIONADOS'
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

IF rb_1.Checked THEN
	li_filtra = 0
ELSEIF rb_2.Checked THEN
	li_filtra = 1
ELSE
	li_filtra = -1
END IF	

IF cbx_3.Checked THEN
	li_sag = 2
ELSE
	li_sag = 1
END IF	

vinf.dw_1.DataObject = "dw_info_repalletizadogenerado"

vinf.dw_1.SetTransObject(sqlca)

ll_ndesde = Long(em_ndesde.Text)
ll_nhasta = Long(em_nhasta.Text)
ld_fdesde = Date(em_fdesde.Text)
ld_fhasta = Date(em_fhasta.Text)

ll_Fila	=	vinf.dw_1.Retrieve(ii_Cliente, ii_Planta, ll_ndesde, ll_nhasta, ld_fdesde, ld_fhasta, li_sag, li_filtra)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify("ndesde.text = '" + String(ll_ndesde) + "'")
	vinf.dw_1.Modify("nhasta.text = '" + String(ll_nhasta) + "'")
	vinf.dw_1.Modify("fdesde.text = '" + String(ld_fdesde) + "'")
	vinf.dw_1.Modify("fhasta.text = '" + String(ld_fhasta) + "'")
		
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
end event

type pb_salir from w_para_informes`pb_salir within w_info_archivorepalletizado_generado
integer x = 2542
integer y = 1236
integer taborder = 170
end type

type dw_cliente from datawindow within w_info_archivorepalletizado_generado
integer x = 891
integer y = 520
integer width = 1157
integer height = 92
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer 	li_nula, li_busca

SetNull(li_nula)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	
	istr_mant.argumento[1]	=	data
	ii_cliente	=	integer(data)

   dw_cliente.Object.clie_codigo[1]	=	integer(data)
   li_busca = idwc_cliente.Find("clie_codigo = " + String(integer(data)), 1, idwc_cliente.RowCount())
   is_NomCliente = idwc_cliente.GetItemString(li_busca, "clie_nombre")

   dw_planta.GetChild("plde_codigo", idwc_planta)
   idwc_planta.SetTransObject(SQLCA)
   idwc_planta.Retrieve(1)
   dw_planta.InsertRow(0)
   dw_planta.Object.plde_codigo[1] = dw_planta.Object.plde_codigo[1]
	istr_mant.argumento[3]	=	String(dw_planta.Object.plde_codigo[1])
	ii_planta	=	integer(istr_mant.argumento[3])	
   li_busca = idwc_planta.Find("plde_codigo = " + String(ii_planta), 1, idwc_planta.RowCount())
   is_NomPlanta = idwc_planta.GetItemString(li_busca, "plde_nombre")

ELSE
	This.SetItem(1, "clie_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_archivorepalletizado_generado
integer x = 338
integer y = 548
integer width = 270
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

type em_fdesde from editmask within w_info_archivorepalletizado_generado
integer x = 891
integer y = 1056
integer width = 402
integer height = 92
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_8 from statictext within w_info_archivorepalletizado_generado
integer x = 338
integer y = 688
integer width = 270
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

type dw_planta from datawindow within w_info_archivorepalletizado_generado
integer x = 891
integer y = 660
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExistePlanta(ii_Cliente,Integer(data), ls_Columna[]) THEN
	ii_Planta		=	Integer(data)
	is_NomPlanta	=	ls_Columna[1]
ELSE
	This.SetItem(1, "plde_codigo", li_Nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type em_fhasta from editmask within w_info_archivorepalletizado_generado
integer x = 1605
integer y = 1056
integer width = 402
integer height = 92
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_14 from statictext within w_info_archivorepalletizado_generado
integer x = 1362
integer y = 1080
integer width = 206
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
string text = "Hasta"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_archivorepalletizado_generado
integer x = 571
integer y = 876
integer width = 256
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
string text = "Desde"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_archivorepalletizado_generado
integer x = 1362
integer y = 876
integer width = 206
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
string text = "Hasta"
boolean focusrectangle = false
end type

type em_ndesde from editmask within w_info_archivorepalletizado_generado
integer x = 896
integer y = 852
integer width = 402
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type em_nhasta from editmask within w_info_archivorepalletizado_generado
integer x = 1605
integer y = 856
integer width = 402
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type gb_3 from groupbox within w_info_archivorepalletizado_generado
integer x = 265
integer y = 788
integer width = 2126
integer height = 188
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nº Repalletizado"
end type

type gb_4 from groupbox within w_info_archivorepalletizado_generado
integer x = 265
integer y = 1172
integer width = 2126
integer height = 172
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type st_12 from statictext within w_info_archivorepalletizado_generado
integer x = 571
integer y = 1080
integer width = 256
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
string text = "Desde"
boolean focusrectangle = false
end type

type cbx_cliente from checkbox within w_info_archivorepalletizado_generado
integer x = 2053
integer y = 532
integer width = 311
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
string text = "Todos"
boolean checked = true
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_Cliente = -1
	dw_cliente.SetItem(1,"clie_codigo",li_null)
	dw_cliente.Object.clie_codigo.Protect	=	1
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	dw_cliente.Object.clie_codigo.Protect	=	0
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_cliente.SetFocus()
	ii_Cliente = dw_cliente.Object.clie_codigo[1]
END IF
end event

type cbx_planta from checkbox within w_info_archivorepalletizado_generado
integer x = 2053
integer y = 672
integer width = 311
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
string text = "Todos"
boolean checked = true
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_Planta = -1
	dw_planta.SetItem(1,"plde_codigo",li_null)
	dw_planta.Object.plde_codigo.Protect	=	1
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	dw_planta.Object.plde_codigo.Protect	=	0
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_planta.SetFocus()
	ii_Planta = dw_planta.Object.plde_codigo[1]
END IF
end event

type cbx_1 from checkbox within w_info_archivorepalletizado_generado
integer x = 2053
integer y = 868
integer width = 311
integer height = 76
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
string text = "Todos"
boolean checked = true
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	em_ndesde.Text = '1'
	em_nhasta.Text = '99999999'
	em_ndesde.Enabled = False
	em_nhasta.Enabled = False
ELSE
	em_ndesde.Enabled = True
	em_nhasta.Enabled = True
	em_ndesde.SetFocus()
END IF
end event

type cbx_2 from checkbox within w_info_archivorepalletizado_generado
integer x = 2053
integer y = 1060
integer width = 311
integer height = 76
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
string text = "Todos"
end type

event clicked;
IF This.Checked THEN
	em_fdesde.Text = '19000101'
	em_fhasta.Text = String(Today())
	em_fdesde.Enabled = False
	em_fhasta.Enabled = False

ELSE
	em_fdesde.Enabled = True
	em_fhasta.Enabled = True
	em_fhasta.text					=	String(RelativeDate(Today() , -365))
	em_fhasta.text					=	String(Today())	
	em_fdesde.SetFocus()
END IF
end event

type gb_6 from groupbox within w_info_archivorepalletizado_generado
integer x = 265
integer y = 996
integer width = 2126
integer height = 172
integer taborder = 180
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Repalletizado"
end type

type st_1 from statictext within w_info_archivorepalletizado_generado
integer x = 251
integer y = 440
integer width = 2171
integer height = 1036
integer textsize = -10
integer weight = 400
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

type rb_1 from radiobutton within w_info_archivorepalletizado_generado
integer x = 517
integer y = 1232
integer width = 658
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "No Generados"
boolean checked = true
end type

type rb_2 from radiobutton within w_info_archivorepalletizado_generado
integer x = 1175
integer y = 1232
integer width = 581
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Generados"
end type

type rb_3 from radiobutton within w_info_archivorepalletizado_generado
integer x = 1751
integer y = 1232
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
end type

type cbx_3 from checkbox within w_info_archivorepalletizado_generado
integer x = 942
integer y = 1368
integer width = 887
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Solo con Número de Sag"
boolean checked = true
end type

