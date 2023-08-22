$PBExportHeader$w_info_inspeccion_rechazada.srw
forward
global type w_info_inspeccion_rechazada from w_para_informes
end type
type st_1 from statictext within w_info_inspeccion_rechazada
end type
type st_2 from statictext within w_info_inspeccion_rechazada
end type
type dw_2 from datawindow within w_info_inspeccion_rechazada
end type
type st_6 from statictext within w_info_inspeccion_rechazada
end type
type dw_1 from datawindow within w_info_inspeccion_rechazada
end type
type em_nroguia from editmask within w_info_inspeccion_rechazada
end type
type st_3 from statictext within w_info_inspeccion_rechazada
end type
type st_14 from statictext within w_info_inspeccion_rechazada
end type
type em_fecha from editmask within w_info_inspeccion_rechazada
end type
type em_loteori from editmask within w_info_inspeccion_rechazada
end type
type st_15 from statictext within w_info_inspeccion_rechazada
end type
type em_rechazo from editmask within w_info_inspeccion_rechazada
end type
type st_16 from statictext within w_info_inspeccion_rechazada
end type
type st_4 from statictext within w_info_inspeccion_rechazada
end type
type em_4 from editmask within w_info_inspeccion_rechazada
end type
type em_5 from editmask within w_info_inspeccion_rechazada
end type
type em_6 from editmask within w_info_inspeccion_rechazada
end type
type em_cam1 from editmask within w_info_inspeccion_rechazada
end type
type em_pal1 from editmask within w_info_inspeccion_rechazada
end type
type em_caj1 from editmask within w_info_inspeccion_rechazada
end type
type em_cam2 from editmask within w_info_inspeccion_rechazada
end type
type em_pal2 from editmask within w_info_inspeccion_rechazada
end type
type em_caj2 from editmask within w_info_inspeccion_rechazada
end type
type em_cam3 from editmask within w_info_inspeccion_rechazada
end type
type em_pal3 from editmask within w_info_inspeccion_rechazada
end type
type em_caj3 from editmask within w_info_inspeccion_rechazada
end type
type ddlb_accion from dropdownlistbox within w_info_inspeccion_rechazada
end type
type st_5 from statictext within w_info_inspeccion_rechazada
end type
type st_7 from statictext within w_info_inspeccion_rechazada
end type
type dw_3 from datawindow within w_info_inspeccion_rechazada
end type
type em_contraparte from editmask within w_info_inspeccion_rechazada
end type
type st_8 from statictext within w_info_inspeccion_rechazada
end type
type st_9 from statictext within w_info_inspeccion_rechazada
end type
type em_aprueba from editmask within w_info_inspeccion_rechazada
end type
type st_10 from statictext within w_info_inspeccion_rechazada
end type
type rb_1 from radiobutton within w_info_inspeccion_rechazada
end type
type rb_2 from radiobutton within w_info_inspeccion_rechazada
end type
end forward

global type w_info_inspeccion_rechazada from w_para_informes
integer x = 14
integer y = 32
integer width = 2729
integer height = 2032
string title = "Registro Lote Rechazado"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
dw_2 dw_2
st_6 st_6
dw_1 dw_1
em_nroguia em_nroguia
st_3 st_3
st_14 st_14
em_fecha em_fecha
em_loteori em_loteori
st_15 st_15
em_rechazo em_rechazo
st_16 st_16
st_4 st_4
em_4 em_4
em_5 em_5
em_6 em_6
em_cam1 em_cam1
em_pal1 em_pal1
em_caj1 em_caj1
em_cam2 em_cam2
em_pal2 em_pal2
em_caj2 em_caj2
em_cam3 em_cam3
em_pal3 em_pal3
em_caj3 em_caj3
ddlb_accion ddlb_accion
st_5 st_5
st_7 st_7
dw_3 dw_3
em_contraparte em_contraparte
st_8 st_8
st_9 st_9
em_aprueba em_aprueba
st_10 st_10
rb_1 rb_1
rb_2 rb_2
end type
global w_info_inspeccion_rechazada w_info_inspeccion_rechazada

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo, ii_accion, ii_destino
String	is_report, is_Recibidor, is_Direccion, is_Rut, is_archivo, is_asunto
integer  ii_var, ii_prod, ii_cal, ii_cli, ii_pakrot
DataWindowChild	idwc_destino, idwc_cliente, idwc_planta
end variables

forward prototypes
public function integer despacho (integer li_cliente, integer li_planta, long ll_guia)
public subroutine enviamail ()
public function boolean existeinspeccion (long al_guia)
end prototypes

public function integer despacho (integer li_cliente, integer li_planta, long ll_guia);Long ll_numero
Integer li_contador

SELECT defe_numero
INTO	:ll_numero
FROM	dbo.DESPAFRIGOEN 
WHERE	plde_codigo =	:li_planta
AND	clie_codigo	=	:li_cliente
AND	defe_guides	=	:ll_guia;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
	RETURN 0
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No existe Número Guia Despacho Indicado.~r~rIngrese otro Número.", &
					Exclamation!, Ok!)
	RETURN 0
END IF

SELECT count(*)
INTO :li_contador
FROM dbo.CONTROLDESPACHOS
where plde_codigo =	:li_planta
AND	clie_codigo	=	:li_cliente
AND	defe_numero	=	:ll_numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla CONTROLDESPACHOS")
	RETURN 0
END IF

IF li_contador > 0 THEN
	IF ii_tipo = 1 THEN
		update dbo.CONTROLDESPACHOS Set 
		CODE_GENGDE = 1
   	where  plde_codigo =	:li_planta
		AND	 clie_codigo =	:li_cliente
		AND	 defe_numero =	:ll_numero;
		Commit;
		
		update dbo.CONTROLDESPACHOS Set 
		CODE_GEMIDE = 1
   	where  plde_codigo =	:li_planta
		AND	 clie_codigo =	:li_cliente
		AND	 defe_numero =	:ll_numero;
		Commit;
//	ELSEIF ii_tipo = 2 THEN	
//		update dbo.CONTROLDESPACHOS Set 
//		CODE_GEMIDE = 1
//   	where  plde_codigo =	:li_planta
//		AND	 clie_codigo =	:li_cliente
//		AND	 defe_numero =	:ll_numero;
//		Commit;
	ELSE	
		update dbo.CONTROLDESPACHOS Set 
		CODE_DEMADE = 1
   	where  plde_codigo =	:li_planta
		AND	 clie_codigo =	:li_cliente
		AND	 defe_numero =	:ll_numero;
		Commit;
	END IF	
END IF	
return 0



end function

public subroutine enviamail ();String		ls_Nombre, ls_NomReporte, ls_Archivo, ls_DirectorioAct 
Long			ll_Fila, ll_consignatario, ll_Archivo
Boolean		lb_Existe
Integer		li_imprimio
str_parms	lstr_parms

SetPointer(HourGlass!)

//RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_DirectorioAct)

ls_NomReporte									=	is_archivo
lstr_parms.string_arg[1]					=	String(1)
lstr_parms.string_arg[2]					=	'Guia Despacho Nº '+is_asunto
lstr_parms.string_arg[3]					=	String(1)

ll_Archivo = 1

lstr_parms.string_arg[ll_Archivo+3]		=	is_archivo

//FOR ll_Archivo = 1 TO 8
////	lstr_parms.string_arg[ll_Archivo+4]	=	ls_DirectorioAct + '\' + dw_archivos.Object.Nombre[ll_archivo]
//NEXT

OpenWithParm(w_correo_archivo_saam, lstr_parms)

lb_Existe	=	FileExists(lstr_parms.string_arg[ll_Archivo + 3])
	
IF lb_Existe THEN
	FileDelete(lstr_parms.string_arg[ll_Archivo + 3])
END IF

SetPointer(Arrow!)
end subroutine

public function boolean existeinspeccion (long al_guia);Integer	li_codexp, li_planta, li_Tipova, li_tipoembq, li_embc_codigo, li_tecnic
Date		ld_fecha
String	ls_Embarque, ls_puerto, ls_destino, ls_contraparte

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF al_guia <> 0 OR li_planta = 0 THEN

	SELECT max(inpe_fechai)
		INTO	:ld_fecha
		FROM	dbo.inspecpalenc 
		WHERE	plde_codigo =	:li_planta
		AND	clie_codigo	=	:li_codexp
		AND	inpe_numero	=	:al_guia
		AND	inpe_estado in	(3,1);
	//	AND	isnull(inpe_fhisre,19000101) <> 19000101;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla inspecpalenc")
		em_nroguia.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Número Inpección Rechazada.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_acepta.Enabled	= False
		em_nroguia.SetFocus()
		RETURN False
	ELSE	
		em_fecha.Text = String(ld_fecha)
		em_loteori.Text = String(al_guia)
		RETURN True
	END IF	
ELSE	
	RETURN False
END IF	

end function

on w_info_inspeccion_rechazada.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
this.em_nroguia=create em_nroguia
this.st_3=create st_3
this.st_14=create st_14
this.em_fecha=create em_fecha
this.em_loteori=create em_loteori
this.st_15=create st_15
this.em_rechazo=create em_rechazo
this.st_16=create st_16
this.st_4=create st_4
this.em_4=create em_4
this.em_5=create em_5
this.em_6=create em_6
this.em_cam1=create em_cam1
this.em_pal1=create em_pal1
this.em_caj1=create em_caj1
this.em_cam2=create em_cam2
this.em_pal2=create em_pal2
this.em_caj2=create em_caj2
this.em_cam3=create em_cam3
this.em_pal3=create em_pal3
this.em_caj3=create em_caj3
this.ddlb_accion=create ddlb_accion
this.st_5=create st_5
this.st_7=create st_7
this.dw_3=create dw_3
this.em_contraparte=create em_contraparte
this.st_8=create st_8
this.st_9=create st_9
this.em_aprueba=create em_aprueba
this.st_10=create st_10
this.rb_1=create rb_1
this.rb_2=create rb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_2
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.dw_1
this.Control[iCurrent+6]=this.em_nroguia
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.st_14
this.Control[iCurrent+9]=this.em_fecha
this.Control[iCurrent+10]=this.em_loteori
this.Control[iCurrent+11]=this.st_15
this.Control[iCurrent+12]=this.em_rechazo
this.Control[iCurrent+13]=this.st_16
this.Control[iCurrent+14]=this.st_4
this.Control[iCurrent+15]=this.em_4
this.Control[iCurrent+16]=this.em_5
this.Control[iCurrent+17]=this.em_6
this.Control[iCurrent+18]=this.em_cam1
this.Control[iCurrent+19]=this.em_pal1
this.Control[iCurrent+20]=this.em_caj1
this.Control[iCurrent+21]=this.em_cam2
this.Control[iCurrent+22]=this.em_pal2
this.Control[iCurrent+23]=this.em_caj2
this.Control[iCurrent+24]=this.em_cam3
this.Control[iCurrent+25]=this.em_pal3
this.Control[iCurrent+26]=this.em_caj3
this.Control[iCurrent+27]=this.ddlb_accion
this.Control[iCurrent+28]=this.st_5
this.Control[iCurrent+29]=this.st_7
this.Control[iCurrent+30]=this.dw_3
this.Control[iCurrent+31]=this.em_contraparte
this.Control[iCurrent+32]=this.st_8
this.Control[iCurrent+33]=this.st_9
this.Control[iCurrent+34]=this.em_aprueba
this.Control[iCurrent+35]=this.st_10
this.Control[iCurrent+36]=this.rb_1
this.Control[iCurrent+37]=this.rb_2
end on

on w_info_inspeccion_rechazada.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.em_nroguia)
destroy(this.st_3)
destroy(this.st_14)
destroy(this.em_fecha)
destroy(this.em_loteori)
destroy(this.st_15)
destroy(this.em_rechazo)
destroy(this.st_16)
destroy(this.st_4)
destroy(this.em_4)
destroy(this.em_5)
destroy(this.em_6)
destroy(this.em_cam1)
destroy(this.em_pal1)
destroy(this.em_caj1)
destroy(this.em_cam2)
destroy(this.em_pal2)
destroy(this.em_caj2)
destroy(this.em_cam3)
destroy(this.em_pal3)
destroy(this.em_caj3)
destroy(this.ddlb_accion)
destroy(this.st_5)
destroy(this.st_7)
destroy(this.dw_3)
destroy(this.em_contraparte)
destroy(this.st_8)
destroy(this.st_9)
destroy(this.em_aprueba)
destroy(this.st_10)
destroy(this.rb_1)
destroy(this.rb_2)
end on

event open;call super::open;

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1,"clie_codigo", gi_codexport)

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_1.InsertRow(0)
dw_1.SetItem(1,"plde_codigo", gi_codplanta)

dw_3.GetChild("dest_codigo", idwc_destino)
idwc_destino.SetTransObject(sqlca)
idwc_destino.Retrieve(-1)
dw_3.InsertRow(0)

istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)

end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_inspeccion_rechazada
end type

type st_computador from w_para_informes`st_computador within w_info_inspeccion_rechazada
end type

type st_usuario from w_para_informes`st_usuario within w_info_inspeccion_rechazada
end type

type st_temporada from w_para_informes`st_temporada within w_info_inspeccion_rechazada
end type

type p_logo from w_para_informes`p_logo within w_info_inspeccion_rechazada
end type

type st_titulo from w_para_informes`st_titulo within w_info_inspeccion_rechazada
integer width = 1934
string text = "Registro Lote Rechazado"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_inspeccion_rechazada
integer x = 2331
integer y = 1312
integer taborder = 200
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_planta, li_cliente,li_tipoembq, li_vari, li_emba, li_calib
Long		ll_numero, ll_Fila
String	ls_cliente, ls_planta, ls_DirectorioAct, ls_Archivo
Date		ld_fecha

li_cliente	=	Integer(istr_mant.argumento[1])

istr_info.titulo	= 'EMISION LOTE RECHAZADO'

OpenWithParm(vinf, istr_info)

IF rb_1.Checked THEN
	vinf.dw_1.DataObject = 'dw_info_lotesrechazados'
ELSE
	vinf.dw_1.DataObject = 'dw_info_lotesrechazados032013'
END IF	

vinf.dw_1.SetTransObject(sqlca)

li_planta	=	Integer(istr_mant.argumento[2])
ll_numero	=	Long(em_nroguia.Text)
ld_fecha		=	date(em_fecha.Text)

fila = vinf.dw_1.Retrieve(li_cliente, li_planta, ll_numero,ii_accion,ii_destino)
	
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

 ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify("t_fecha.text = '" + em_fecha.Text + "'")
	vinf.dw_1.Modify("t_lote.text = '" + string(ll_numero) + "'")
	vinf.dw_1.Modify("t_causal.text = '" + em_rechazo.Text + "'")
	
	vinf.dw_1.Modify("t_camara1.text = '" + em_cam1.Text + "'")
	vinf.dw_1.Modify("t_camara2.text = '" + em_cam2.Text + "'")
	vinf.dw_1.Modify("t_camara3.text = '" + em_cam3.Text + "'")
	
	vinf.dw_1.Modify("t_pallet1.text = '" + em_pal1.Text + "'")
	vinf.dw_1.Modify("t_pallet2.text = '" + em_pal2.Text + "'")
	vinf.dw_1.Modify("t_pallet3.text = '" + em_pal3.Text + "'")
	
	vinf.dw_1.Modify("t_caja1.text = '" + em_caj1.Text + "'")
	vinf.dw_1.Modify("t_caja2.text = '" + em_caj2.Text + "'")
	vinf.dw_1.Modify("t_caja3.text = '" + em_caj3.Text + "'")
	
	vinf.dw_1.Modify("t_nombre.text = '" + em_contraparte.Text + "'")
	vinf.dw_1.Modify("t_aprueba.text = '" + em_aprueba.Text + "'")
		
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
	
	vinf.dw_1.GroupCalc()
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_inspeccion_rechazada
integer x = 2336
integer y = 1588
integer taborder = 210
end type

type st_1 from statictext within w_info_inspeccion_rechazada
integer x = 283
integer y = 568
integer width = 462
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_inspeccion_rechazada
integer x = 283
integer y = 680
integer width = 462
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
string text = "Nro. Inspección"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_info_inspeccion_rechazada
integer x = 850
integer y = 448
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	String(data)
idwc_planta.Retrieve(1)
istr_mant.argumento[2]	=	String(dw_1.Object.plde_codigo[1])
dw_1.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
end event

type st_6 from statictext within w_info_inspeccion_rechazada
integer x = 283
integer y = 480
integer width = 233
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

type dw_1 from datawindow within w_info_inspeccion_rechazada
integer x = 850
integer y = 548
integer width = 965
integer height = 96
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]=String(data)
end event

type em_nroguia from editmask within w_info_inspeccion_rechazada
integer x = 850
integer y = 648
integer width = 402
integer height = 96
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF ExisteInspeccion(Long(This.Text)) = False THEN
	This.Text = ''
	This.SetFocus()
END IF
end event

type st_3 from statictext within w_info_inspeccion_rechazada
integer x = 251
integer y = 1188
integer width = 1934
integer height = 488
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_inspeccion_rechazada
integer x = 283
integer y = 788
integer width = 462
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
string text = "Fecha"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_info_inspeccion_rechazada
integer x = 850
integer y = 756
integer width = 402
integer height = 96
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
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
end type

type em_loteori from editmask within w_info_inspeccion_rechazada
integer x = 850
integer y = 864
integer width = 402
integer height = 96
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
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = decimalmask!
string mask = "########"
end type

type st_15 from statictext within w_info_inspeccion_rechazada
integer x = 283
integer y = 896
integer width = 462
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
string text = "Lote Original"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_rechazo from editmask within w_info_inspeccion_rechazada
integer x = 850
integer y = 972
integer width = 1289
integer height = 192
integer taborder = 60
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
maskdatatype maskdatatype = stringmask!
boolean autoskip = true
end type

type st_16 from statictext within w_info_inspeccion_rechazada
integer x = 279
integer y = 1100
integer width = 571
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
string text = "Causal de Rechazo"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_inspeccion_rechazada
integer x = 251
integer y = 432
integer width = 1934
integer height = 756
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_4 from editmask within w_info_inspeccion_rechazada
boolean visible = false
integer x = 2930
integer y = 1024
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Nº Cámara"
alignment alignment = center!
maskdatatype maskdatatype = stringmask!
end type

type em_5 from editmask within w_info_inspeccion_rechazada
boolean visible = false
integer x = 3721
integer y = 1024
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Nº Cajas"
alignment alignment = center!
maskdatatype maskdatatype = stringmask!
end type

type em_6 from editmask within w_info_inspeccion_rechazada
boolean visible = false
integer x = 3323
integer y = 1024
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Nº Pallet"
alignment alignment = center!
maskdatatype maskdatatype = stringmask!
end type

type em_cam1 from editmask within w_info_inspeccion_rechazada
boolean visible = false
integer x = 2770
integer y = 1096
integer width = 402
integer height = 72
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
maskdatatype maskdatatype = stringmask!
end type

type em_pal1 from editmask within w_info_inspeccion_rechazada
boolean visible = false
integer x = 3323
integer y = 1092
integer width = 402
integer height = 72
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
maskdatatype maskdatatype = stringmask!
end type

type em_caj1 from editmask within w_info_inspeccion_rechazada
boolean visible = false
integer x = 3721
integer y = 1092
integer width = 402
integer height = 72
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
maskdatatype maskdatatype = stringmask!
end type

type em_cam2 from editmask within w_info_inspeccion_rechazada
boolean visible = false
integer x = 2930
integer y = 1160
integer width = 402
integer height = 72
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
maskdatatype maskdatatype = stringmask!
end type

type em_pal2 from editmask within w_info_inspeccion_rechazada
boolean visible = false
integer x = 3323
integer y = 1160
integer width = 402
integer height = 72
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
maskdatatype maskdatatype = stringmask!
end type

type em_caj2 from editmask within w_info_inspeccion_rechazada
boolean visible = false
integer x = 3721
integer y = 1160
integer width = 402
integer height = 72
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
maskdatatype maskdatatype = stringmask!
end type

type em_cam3 from editmask within w_info_inspeccion_rechazada
boolean visible = false
integer x = 2930
integer y = 1228
integer width = 402
integer height = 72
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
maskdatatype maskdatatype = stringmask!
end type

type em_pal3 from editmask within w_info_inspeccion_rechazada
boolean visible = false
integer x = 3323
integer y = 1228
integer width = 402
integer height = 72
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
maskdatatype maskdatatype = stringmask!
end type

type em_caj3 from editmask within w_info_inspeccion_rechazada
boolean visible = false
integer x = 3721
integer y = 1228
integer width = 402
integer height = 72
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
maskdatatype maskdatatype = stringmask!
end type

type ddlb_accion from dropdownlistbox within w_info_inspeccion_rechazada
integer x = 850
integer y = 1204
integer width = 1289
integer height = 660
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean sorted = false
boolean hscrollbar = true
boolean vscrollbar = true
string item[] = {"Redestinación a Otro Pais","Eliminación del Productor","Reembalaje ","Fumigación","Devolución a Productor","Mercado Interno","Segregación","Otros"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_accion = index
end event

type st_5 from statictext within w_info_inspeccion_rechazada
integer x = 283
integer y = 1216
integer width = 361
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
string text = "Tipo Acción"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_inspeccion_rechazada
integer x = 283
integer y = 1320
integer width = 549
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
string text = "Pais a Redestinar"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_info_inspeccion_rechazada
integer x = 846
integer y = 1312
integer width = 1102
integer height = 96
integer taborder = 170
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_destinos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;ii_destino = Integer(data)
end event

type em_contraparte from editmask within w_info_inspeccion_rechazada
integer x = 850
integer y = 1420
integer width = 1289
integer height = 96
integer taborder = 180
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
maskdatatype maskdatatype = stringmask!
boolean autoskip = true
end type

type st_8 from statictext within w_info_inspeccion_rechazada
integer x = 283
integer y = 1436
integer width = 375
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
string text = "Contraparte"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_inspeccion_rechazada
integer x = 283
integer y = 1540
integer width = 357
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
string text = "Aprueba"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_aprueba from editmask within w_info_inspeccion_rechazada
integer x = 850
integer y = 1528
integer width = 1289
integer height = 96
integer taborder = 190
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
maskdatatype maskdatatype = stringmask!
boolean autoskip = true
end type

type st_10 from statictext within w_info_inspeccion_rechazada
integer x = 251
integer y = 1676
integer width = 1934
integer height = 136
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_1 from radiobutton within w_info_inspeccion_rechazada
integer x = 485
integer y = 1704
integer width = 526
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
string text = "Formato Actual"
boolean checked = true
boolean lefttext = true
end type

type rb_2 from radiobutton within w_info_inspeccion_rechazada
integer x = 1303
integer y = 1700
integer width = 704
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
string text = "Formato Marzo 2013"
boolean lefttext = true
end type

