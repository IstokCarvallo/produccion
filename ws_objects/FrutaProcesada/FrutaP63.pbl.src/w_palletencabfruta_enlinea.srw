$PBExportHeader$w_palletencabfruta_enlinea.srw
forward
global type w_palletencabfruta_enlinea from w_para_informes
end type
type dw_stat from datawindow within w_palletencabfruta_enlinea
end type
type dw_1 from datawindow within w_palletencabfruta_enlinea
end type
type dw_2 from datawindow within w_palletencabfruta_enlinea
end type
type dw_3 from datawindow within w_palletencabfruta_enlinea
end type
type dw_4 from datawindow within w_palletencabfruta_enlinea
end type
type dw_5 from datawindow within w_palletencabfruta_enlinea
end type
type dw_6 from datawindow within w_palletencabfruta_enlinea
end type
type st_9 from statictext within w_palletencabfruta_enlinea
end type
type cbx_archivo from checkbox within w_palletencabfruta_enlinea
end type
type dw_excel from datawindow within w_palletencabfruta_enlinea
end type
type st_1 from statictext within w_palletencabfruta_enlinea
end type
end forward

global type w_palletencabfruta_enlinea from w_para_informes
integer x = 14
integer y = 32
integer width = 2181
integer height = 1220
string title = "Genera Existencia de Linea"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_stat dw_stat
dw_1 dw_1
dw_2 dw_2
dw_3 dw_3
dw_4 dw_4
dw_5 dw_5
dw_6 dw_6
st_9 st_9
cbx_archivo cbx_archivo
dw_excel dw_excel
st_1 st_1
end type
global w_palletencabfruta_enlinea w_palletencabfruta_enlinea

type variables
str_busqueda 	istr_busq
str_mant 		istr_mant

String is_control_d
Integer ii_control, ii_calificacion

DataWindowChild	idwc_cliente, idwc_especie, idwc_stat, idwc_categorias

uo_seleccion_especie					iuo_selespecie
uo_seleccion_variedad				iuo_selvariedad
uo_seleccion_varios_productores	iuo_selproductor

end variables

on w_palletencabfruta_enlinea.create
int iCurrent
call super::create
this.dw_stat=create dw_stat
this.dw_1=create dw_1
this.dw_2=create dw_2
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_6=create dw_6
this.st_9=create st_9
this.cbx_archivo=create cbx_archivo
this.dw_excel=create dw_excel
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_stat
this.Control[iCurrent+2]=this.dw_1
this.Control[iCurrent+3]=this.dw_2
this.Control[iCurrent+4]=this.dw_3
this.Control[iCurrent+5]=this.dw_4
this.Control[iCurrent+6]=this.dw_5
this.Control[iCurrent+7]=this.dw_6
this.Control[iCurrent+8]=this.st_9
this.Control[iCurrent+9]=this.cbx_archivo
this.Control[iCurrent+10]=this.dw_excel
this.Control[iCurrent+11]=this.st_1
end on

on w_palletencabfruta_enlinea.destroy
call super::destroy
destroy(this.dw_stat)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_6)
destroy(this.st_9)
destroy(this.cbx_archivo)
destroy(this.dw_excel)
destroy(this.st_1)
end on

event open;call super::open;Boolean lb_Cerrar


end event

event resize;call super::resize;//pb_acepta.x			=	This.WorkSpaceWidth() - 292
//pb_acepta.y			=	gb_1.y + 88
//pb_acepta.width	=	156
//pb_acepta.height	=	133
end event

type st_computador from w_para_informes`st_computador within w_palletencabfruta_enlinea
end type

type st_usuario from w_para_informes`st_usuario within w_palletencabfruta_enlinea
end type

type st_temporada from w_para_informes`st_temporada within w_palletencabfruta_enlinea
end type

type p_logo from w_para_informes`p_logo within w_palletencabfruta_enlinea
end type

type st_titulo from w_para_informes`st_titulo within w_palletencabfruta_enlinea
integer width = 1390
string text = "Genera Existencia de Linea"
end type

type pb_acepta from w_para_informes`pb_acepta within w_palletencabfruta_enlinea
string tag = "Imprimir Reporte"
integer x = 1806
integer y = 500
integer taborder = 90
string picturename = "\Desarrollo 12\Imagenes\Botones\GrabaEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\GrabaDisab.png"
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila
String	ls_descri, ls_Archivo, ls_ruta, ls_planta, ls_hora
Datetime	ldt_horcarga

ls_planta	=	String(gi_codplanta)

SELECT Max(pafr_horcar) into :ldt_horcarga
FROM dba.palletencabfruta_enlinea;

istr_info.titulo		= 'GENERA EXISTENCIA EN LINEA'

OpenWithParm(vinf, istr_info)

IF cbx_archivo.Checked THEN

	//dw_excel.DataObject = "dw_palletencabfruta_enlinea"

   dw_excel.SetTransObject(sqlca)

	fila	=	dw_excel.Retrieve(ldt_horcarga)
	
	IF fila > 0 THEN
		
		ls_hora = String(ldt_horcarga,'')
		
		ls_hora = f_global_replace(f_global_replace(ls_hora,':','.'),'/','-')
		
		ls_Archivo	=	"\ExistenciaEnLinea "+ls_planta+" "+String(ls_hora)+".csv"
			
		RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
				
		dw_excel.SaveAs(ls_Ruta + ls_Archivo,csv!, False)
		MessageBox("Atención","Archivo Formato Excel, Generado.")
	
		st_1.Text	=	'Archivo Generado '+String(ls_hora)
		
//		
	END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_palletencabfruta_enlinea
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1810
integer y = 788
integer taborder = 100
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_stat from datawindow within w_palletencabfruta_enlinea
boolean visible = false
integer x = 178
integer y = 2072
integer width = 969
integer height = 88
integer taborder = 100
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[6]	=	data
istr_mant.argumento[7]	=  f_statnombre(integer(data))

end event

type dw_1 from datawindow within w_palletencabfruta_enlinea
boolean visible = false
integer x = 1486
integer y = 1996
integer width = 635
integer height = 160
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_gene_existencia_frigovari"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_palletencabfruta_enlinea
boolean visible = false
integer x = 215
integer y = 2220
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificopredios"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_palletencabfruta_enlinea
boolean visible = false
integer x = 567
integer y = 2220
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_palletencabfruta_enlinea
boolean visible = false
integer x = 791
integer y = 2220
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificos_palletpucho"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_palletencabfruta_enlinea
boolean visible = false
integer x = 1115
integer y = 2220
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificos_pallet_inspec"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_palletencabfruta_enlinea
boolean visible = false
integer x = 1550
integer y = 2220
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificos_pallet_condi"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_9 from statictext within w_palletencabfruta_enlinea
integer x = 251
integer y = 440
integer width = 1390
integer height = 464
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

type cbx_archivo from checkbox within w_palletencabfruta_enlinea
integer x = 590
integer y = 688
integer width = 745
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
long backcolor = 33543637
boolean enabled = false
string text = "Genera Archivo Excel"
boolean checked = true
end type

type dw_excel from datawindow within w_palletencabfruta_enlinea
boolean visible = false
integer x = 1801
integer y = 1148
integer width = 686
integer height = 400
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletencabfruta_enlinea"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_palletencabfruta_enlinea
integer x = 251
integer y = 912
integer width = 1390
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

