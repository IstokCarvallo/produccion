$PBExportHeader$w_info_solicitud_inspeccion.srw
$PBExportComments$Emisión Solicitud de Inspección.
forward
global type w_info_solicitud_inspeccion from w_para_informes
end type
type st_1 from statictext within w_info_solicitud_inspeccion
end type
type dw_cliente from datawindow within w_info_solicitud_inspeccion
end type
type st_2 from statictext within w_info_solicitud_inspeccion
end type
type dw_plantadesp from datawindow within w_info_solicitud_inspeccion
end type
type st_4 from statictext within w_info_solicitud_inspeccion
end type
type ddlb_tipocond from dropdownlistbox within w_info_solicitud_inspeccion
end type
type em_numero from editmask within w_info_solicitud_inspeccion
end type
type st_3 from statictext within w_info_solicitud_inspeccion
end type
type st_5 from statictext within w_info_solicitud_inspeccion
end type
type cbx_terceros from checkbox within w_info_solicitud_inspeccion
end type
type st_6 from statictext within w_info_solicitud_inspeccion
end type
type cbx_varrot from checkbox within w_info_solicitud_inspeccion
end type
type st_7 from statictext within w_info_solicitud_inspeccion
end type
type gb_3 from groupbox within w_info_solicitud_inspeccion
end type
type st_8 from statictext within w_info_solicitud_inspeccion
end type
type rb_aconcagua from radiobutton within w_info_solicitud_inspeccion
end type
type rb_2 from radiobutton within w_info_solicitud_inspeccion
end type
type rb_1 from radiobutton within w_info_solicitud_inspeccion
end type
type cbx_prdrot from checkbox within w_info_solicitud_inspeccion
end type
type cbx_calrot from checkbox within w_info_solicitud_inspeccion
end type
type st_9 from statictext within w_info_solicitud_inspeccion
end type
type cbx_archivo from checkbox within w_info_solicitud_inspeccion
end type
type cbx_packrot from checkbox within w_info_solicitud_inspeccion
end type
type rb_3 from radiobutton within w_info_solicitud_inspeccion
end type
type cbx_1 from checkbox within w_info_solicitud_inspeccion
end type
type cbx_mexicopredios from checkbox within w_info_solicitud_inspeccion
end type
end forward

global type w_info_solicitud_inspeccion from w_para_informes
integer width = 2775
integer height = 2408
string title = "SOLICITUD DE INSPECCION"
st_1 st_1
dw_cliente dw_cliente
st_2 st_2
dw_plantadesp dw_plantadesp
st_4 st_4
ddlb_tipocond ddlb_tipocond
em_numero em_numero
st_3 st_3
st_5 st_5
cbx_terceros cbx_terceros
st_6 st_6
cbx_varrot cbx_varrot
st_7 st_7
gb_3 gb_3
st_8 st_8
rb_aconcagua rb_aconcagua
rb_2 rb_2
rb_1 rb_1
cbx_prdrot cbx_prdrot
cbx_calrot cbx_calrot
st_9 st_9
cbx_archivo cbx_archivo
cbx_packrot cbx_packrot
rb_3 rb_3
cbx_1 cbx_1
cbx_mexicopredios cbx_mexicopredios
end type
global w_info_solicitud_inspeccion w_info_solicitud_inspeccion

type variables
Str_mant				istr_mant
DataWindowChild	dwc_plantas
integer  ii_var
end variables

on w_info_solicitud_inspeccion.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.dw_plantadesp=create dw_plantadesp
this.st_4=create st_4
this.ddlb_tipocond=create ddlb_tipocond
this.em_numero=create em_numero
this.st_3=create st_3
this.st_5=create st_5
this.cbx_terceros=create cbx_terceros
this.st_6=create st_6
this.cbx_varrot=create cbx_varrot
this.st_7=create st_7
this.gb_3=create gb_3
this.st_8=create st_8
this.rb_aconcagua=create rb_aconcagua
this.rb_2=create rb_2
this.rb_1=create rb_1
this.cbx_prdrot=create cbx_prdrot
this.cbx_calrot=create cbx_calrot
this.st_9=create st_9
this.cbx_archivo=create cbx_archivo
this.cbx_packrot=create cbx_packrot
this.rb_3=create rb_3
this.cbx_1=create cbx_1
this.cbx_mexicopredios=create cbx_mexicopredios
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_plantadesp
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.ddlb_tipocond
this.Control[iCurrent+7]=this.em_numero
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.cbx_terceros
this.Control[iCurrent+11]=this.st_6
this.Control[iCurrent+12]=this.cbx_varrot
this.Control[iCurrent+13]=this.st_7
this.Control[iCurrent+14]=this.gb_3
this.Control[iCurrent+15]=this.st_8
this.Control[iCurrent+16]=this.rb_aconcagua
this.Control[iCurrent+17]=this.rb_2
this.Control[iCurrent+18]=this.rb_1
this.Control[iCurrent+19]=this.cbx_prdrot
this.Control[iCurrent+20]=this.cbx_calrot
this.Control[iCurrent+21]=this.st_9
this.Control[iCurrent+22]=this.cbx_archivo
this.Control[iCurrent+23]=this.cbx_packrot
this.Control[iCurrent+24]=this.rb_3
this.Control[iCurrent+25]=this.cbx_1
this.Control[iCurrent+26]=this.cbx_mexicopredios
end on

on w_info_solicitud_inspeccion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.dw_plantadesp)
destroy(this.st_4)
destroy(this.ddlb_tipocond)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.cbx_terceros)
destroy(this.st_6)
destroy(this.cbx_varrot)
destroy(this.st_7)
destroy(this.gb_3)
destroy(this.st_8)
destroy(this.rb_aconcagua)
destroy(this.rb_2)
destroy(this.rb_1)
destroy(this.cbx_prdrot)
destroy(this.cbx_calrot)
destroy(this.st_9)
destroy(this.cbx_archivo)
destroy(this.cbx_packrot)
destroy(this.rb_3)
destroy(this.cbx_1)
destroy(this.cbx_mexicopredios)
end on

event open;call super::open;IF gi_vari_rotulada = 1 THEN
	cbx_varrot.Checked	= True
	cbx_varrot.Enabled	= False
ELSE
	cbx_varrot.Checked	= False
	cbx_varrot.Enabled	= True
END IF	

IF gi_prod_rotulado = 1 THEN
	cbx_prdrot.Checked	=	True
	cbx_prdrot.Enabled	=	False
ELSE
	cbx_prdrot.Checked	= 	False
	cbx_prdrot.Enabled	=	True
END IF

IF gi_cali_rotulado = 1 THEN
	cbx_calrot.Checked	=	True
	cbx_calrot.Enabled	=	False
ELSE
	cbx_calrot.Checked	= 	False
	cbx_calrot.Enabled	=	True
END IF

IF gi_pack_rotulado = 1 THEN
	cbx_packrot.Checked	=	True
	cbx_packrot.Enabled	=	False
ELSE
	cbx_packrot.Checked	= 	False
	cbx_packrot.Enabled	=	True
END IF

ii_var		= gi_vari_rotulada

dw_cliente.SetTransObject(Sqlca)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_plantadesp.GetChild("plde_codigo", dwc_plantas)
dwc_plantas.SetTransObject(Sqlca)
dwc_plantas.Retrieve(1)			//Plantas de Despacho

dw_plantadesp.SetTransObject(Sqlca)
dw_plantadesp.InsertRow(0)
dw_plantadesp.SetItem(1,"plde_codigo", gi_CodPlanta)

ddlb_tipocond.SelectItem(1)

istr_mant.Argumento[1]	=	String(gi_CodExport)
istr_mant.Argumento[2]	=	String(gi_CodPlanta)
istr_mant.Argumento[3]	=	"1"

ids_archivo2					=	CREATE	DataStore
ids_archivo					=	CREATE	DataStore

end event

event resize;//
end event

type st_computador from w_para_informes`st_computador within w_info_solicitud_inspeccion
end type

type st_usuario from w_para_informes`st_usuario within w_info_solicitud_inspeccion
end type

type st_temporada from w_para_informes`st_temporada within w_info_solicitud_inspeccion
end type

type p_logo from w_para_informes`p_logo within w_info_solicitud_inspeccion
end type

type st_titulo from w_para_informes`st_titulo within w_info_solicitud_inspeccion
integer width = 1989
string text = "Emisión Solicitud de Inspección"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_solicitud_inspeccion
integer x = 2400
integer y = 1568
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Filas, li_prdrot, li_calrot, ll_numero
Integer	li_varrot, li_Cliente, li_info, li_packrot, li_tipo, li_planta
String	ls_Archivo, ls_Ruta, ls_Archivo2

str_info	lstr_info

IF em_numero.Text = "" THEN RETURN

lstr_info.titulo	= "SOLICITUD INSPECCION FITOSANITARIA S.A.G."
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

IF rb_aconcagua.Checked THEN
	vinf.dw_1.DataObject = "dw_info_solicitud_inspeccion_aconcagua"
	li_info	=	1
	ids_archivo.DataObject	=	'dw_info_solicitud_inspeccion_acondet_pro'
	ids_archivo.SetTransObject(sqlca)
	ids_archivo2.DataObject	=	'dw_info_solicitud_fitosan_acon_sag'
	ids_archivo2.SetTransObject(sqlca)

ELSEIF rb_2.Checked THEN
	vinf.dw_1.DataObject = "dw_info_solicitud_inspeccion_compuesto"
	li_info	=	2
	ids_archivo.DataObject	=	'dw_info_solicitud_inspeccion_detalle_pro'
	ids_archivo.SetTransObject(sqlca)
	ids_archivo2.DataObject	=	'dw_info_solicitud_fitosanitaria_sag'
	ids_archivo2.SetTransObject(sqlca)

ELSEIF rb_3.Checked THEN
	IF cbx_mexicopredios.checked THEN	
		vinf.dw_1.DataObject = "dw_info_solicitud_inspeccion_sag_mexico"
	ELSE
		vinf.dw_1.DataObject = "dw_info_solicitud_inspeccion_sag_mexico2"
	END IF
	
	li_info	=	4
	ids_archivo.DataObject	=	'dw_info_solicitud_inspeccion_unisag_pro'
	ids_archivo.SetTransObject(sqlca)
	ids_archivo2.DataObject	=	'dw_info_solicitud_fitosan_unisag_sag'
	ids_archivo2.SetTransObject(sqlca)

ELSE	
	vinf.dw_1.DataObject = "dw_info_solicitud_inspeccion_unisag"
	li_info	=	3
	ids_archivo.DataObject	=	'dw_info_solicitud_inspeccion_unisag_pro'
	ids_archivo.SetTransObject(sqlca)
	ids_archivo2.DataObject	=	'dw_info_solicitud_fitosan_unisag_sag'
	ids_archivo2.SetTransObject(sqlca)

END IF

ls_Archivo	=	"\Inspeccion-"+String(Long(istr_mant.argumento[4]))+"Listado.xls"
ls_Archivo2	=	"\Inspeccion-"+String(Long(istr_mant.argumento[4]))+"Fitosanitaria.xls"

vinf.dw_1.SetTransObject(sqlca)

IF cbx_varrot.Checked THEN
	li_varrot 	=	1
ELSE
	li_varrot	=	0
END IF

IF cbx_prdrot.Checked THEN
	li_prdrot 	=	1
ELSE
	li_prdrot	=	0
END IF

IF rb_aconcagua.Checked THEN
	li_calrot	=	-9
END IF

IF cbx_calrot.Checked THEN
	li_calrot 	=	1
ELSE
	li_calrot	=	0
END IF

IF cbx_terceros.checked THEN
	li_Cliente = -1
ELSE
	li_Cliente = dw_cliente.Object.clie_codigo[1]
END IF

IF cbx_packrot.Checked THEN
	li_packrot 	=	1
ELSE
	li_packrot	=	0
END IF

li_tipo		= Integer(istr_mant.argumento[3])
ll_numero	= Long(istr_mant.argumento[4])
li_planta 	= Integer(istr_mant.argumento[2])

/*DECLARE actualiza_registros PROCEDURE FOR dba.FProc_regulariza_cajashistoria_por_inspecion
		@Cliente 	= :li_cliente,
		@Planta  	= :li_planta,
		@Tipo 		= :li_tipo,
		@Inspeccion = :ll_numero,
		@Varrot 		= :li_varrot,
		@prdrot 		= :li_prdrot,
		@calrot 		= :li_calrot;
EXECUTE actualiza_registros ;
CLOSE actualiza_registros; commit;*/

ll_Filas = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]), &
										Long(istr_mant.argumento[4]), &
										li_Cliente, Integer(istr_mant.argumento[2]),li_varrot,&
										li_info,li_prdrot,li_calrot,li_packrot)
								  
IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
//	vinf.dw_1.Object.dw_detalle.Object.nom_empresa.text	= gstr_apl.nom_empresa
//	vinf.dw_1.Object.dw_detalle.Object.rut_empresa.text	= 'R.U.T. ' + String(Double(Mid(gstr_apl.rut_empresa,1,9)),'000,000,000') + '-' + Mid(gstr_apl.rut_empresa,10,1)
//	vinf.dw_1.Object.dw_detalle.Object.dir_empresa.text	= gstr_apl.dir_empresa
	IF cbx_archivo.Checked THEN
		ids_archivo.Retrieve(li_Cliente,Long(istr_mant.argumento[2]), &
								Integer(istr_mant.argumento[3]),long(istr_mant.argumento[4]),li_varrot,&
										li_info,li_prdrot,li_calrot,li_packrot)
		
		ids_archivo2.Retrieve(li_Cliente,Long(istr_mant.argumento[2]), &
								Integer(istr_mant.argumento[3]),long(istr_mant.argumento[4]),li_varrot,&
										li_prdrot,li_calrot)
		
		RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
				
		ids_archivo.SaveAs(ls_Ruta + ls_Archivo,excel5!, True)
		ids_archivo2.SaveAs(ls_Ruta + ls_Archivo2,excel5!, True)
				
		MessageBox("Atención","Archivo Formato Excel, Generado.")
	END IF	
		
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_solicitud_inspeccion
integer x = 2395
integer y = 1844
integer taborder = 60
end type

type st_1 from statictext within w_info_solicitud_inspeccion
integer x = 343
integer y = 532
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_solicitud_inspeccion
integer x = 690
integer y = 532
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1] = data
	dwc_plantas.Retrieve(1)	
ELSE
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_solicitud_inspeccion
integer x = 343
integer y = 668
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_plantadesp from datawindow within w_info_solicitud_inspeccion
integer x = 690
integer y = 668
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExistePlanta(Integer(istr_mant.Argumento[1]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2] = data
ELSE
	RETURN 1
END IF
end event

type st_4 from statictext within w_info_solicitud_inspeccion
integer x = 343
integer y = 804
integer width = 334
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Condición"
boolean focusrectangle = false
end type

type ddlb_tipocond from dropdownlistbox within w_info_solicitud_inspeccion
integer x = 690
integer y = 804
integer width = 526
integer height = 292
integer taborder = 30
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
boolean sorted = false
string item[] = {"Inspección","Re-Inspección"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;istr_mant.argumento[3]	=	String(index)
end event

type em_numero from editmask within w_info_solicitud_inspeccion
integer x = 690
integer y = 944
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
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[4]	=	This.Text
end event

type st_3 from statictext within w_info_solicitud_inspeccion
integer x = 343
integer y = 944
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Número"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_solicitud_inspeccion
integer x = 251
integer y = 440
integer width = 1989
integer height = 624
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_terceros from checkbox within w_info_solicitud_inspeccion
integer x = 686
integer y = 1080
integer width = 1253
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Incluye Terceros mismas Características"
end type

event clicked;IF This.Checked THEN
	dw_cliente.Enabled		=	False
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[1]	=	'-1'
ELSE
	dw_cliente.Enabled		=	True
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255, 255, 255)
	istr_mant.argumento[1]	=	String(dw_cliente.Object.clie_codigo[1])
END IF
end event

type st_6 from statictext within w_info_solicitud_inspeccion
integer x = 251
integer y = 1064
integer width = 1989
integer height = 140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_varrot from checkbox within w_info_solicitud_inspeccion
integer x = 343
integer y = 1224
integer width = 649
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Variedad Rotulada"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_cliente.Enabled		=	False
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[1]	=	'-1'
ELSE
	dw_cliente.Enabled		=	True
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255, 255, 255)
	istr_mant.argumento[1]	=	String(dw_cliente.Object.clie_codigo[1])
END IF
end event

type st_7 from statictext within w_info_solicitud_inspeccion
integer x = 251
integer y = 1208
integer width = 1989
integer height = 272
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_solicitud_inspeccion
integer x = 302
integer y = 1492
integer width = 1888
integer height = 436
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type st_8 from statictext within w_info_solicitud_inspeccion
integer x = 251
integer y = 1480
integer width = 1989
integer height = 480
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_aconcagua from radiobutton within w_info_solicitud_inspeccion
integer x = 585
integer y = 1540
integer width = 690
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Formato Aconcagua"
boolean checked = true
end type

type rb_2 from radiobutton within w_info_solicitud_inspeccion
integer x = 585
integer y = 1632
integer width = 635
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Formato Genérico"
end type

type rb_1 from radiobutton within w_info_solicitud_inspeccion
integer x = 585
integer y = 1724
integer width = 709
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Formato Único S.A.G."
end type

type cbx_prdrot from checkbox within w_info_solicitud_inspeccion
integer x = 1271
integer y = 1224
integer width = 850
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Productor Rotulado"
boolean checked = true
end type

type cbx_calrot from checkbox within w_info_solicitud_inspeccion
integer x = 1271
integer y = 1320
integer width = 850
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Calibre Rotulado Repa  "
end type

type st_9 from statictext within w_info_solicitud_inspeccion
integer x = 251
integer y = 1960
integer width = 1989
integer height = 144
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_archivo from checkbox within w_info_solicitud_inspeccion
integer x = 1088
integer y = 1988
integer width = 320
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Archivo"
end type

type cbx_packrot from checkbox within w_info_solicitud_inspeccion
integer x = 343
integer y = 1320
integer width = 695
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Packing Rotulado"
end type

type rb_3 from radiobutton within w_info_solicitud_inspeccion
integer x = 585
integer y = 1816
integer width = 709
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Formato Mexico"
end type

type cbx_1 from checkbox within w_info_solicitud_inspeccion
boolean visible = false
integer x = 2638
integer y = 948
integer width = 850
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Productor Rotulado"
boolean checked = true
end type

type cbx_mexicopredios from checkbox within w_info_solicitud_inspeccion
integer x = 1271
integer y = 1804
integer width = 850
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Incluye Predios"
end type

