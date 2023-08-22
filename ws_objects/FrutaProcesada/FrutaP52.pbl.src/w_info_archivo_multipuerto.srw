$PBExportHeader$w_info_archivo_multipuerto.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_archivo_multipuerto from w_para_informes
end type
type st_7 from statictext within w_info_archivo_multipuerto
end type
type em_fecha from editmask within w_info_archivo_multipuerto
end type
type st_4 from statictext within w_info_archivo_multipuerto
end type
type st_3 from statictext within w_info_archivo_multipuerto
end type
type em_planilla from editmask within w_info_archivo_multipuerto
end type
type st_2 from statictext within w_info_archivo_multipuerto
end type
type sle_mensa from singlelineedit within w_info_archivo_multipuerto
end type
type st_1 from statictext within w_info_archivo_multipuerto
end type
type st_6 from statictext within w_info_archivo_multipuerto
end type
type dw_1 from datawindow within w_info_archivo_multipuerto
end type
type cb_valida from commandbutton within w_info_archivo_multipuerto
end type
type dw_3 from datawindow within w_info_archivo_multipuerto
end type
type st_5 from statictext within w_info_archivo_multipuerto
end type
type st_8 from statictext within w_info_archivo_multipuerto
end type
type ddlb_1 from dropdownlistbox within w_info_archivo_multipuerto
end type
type uo_selplantas from uo_seleccion_plantas within w_info_archivo_multipuerto
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_archivo_multipuerto
end type
type uo_seltiposag from uo_seleccion_tiposag within w_info_archivo_multipuerto
end type
end forward

global type w_info_archivo_multipuerto from w_para_informes
integer width = 2565
integer height = 1616
event ue_listo ( )
st_7 st_7
em_fecha em_fecha
st_4 st_4
st_3 st_3
em_planilla em_planilla
st_2 st_2
sle_mensa sle_mensa
st_1 st_1
st_6 st_6
dw_1 dw_1
cb_valida cb_valida
dw_3 dw_3
st_5 st_5
st_8 st_8
ddlb_1 ddlb_1
uo_selplantas uo_selplantas
uo_selcliente uo_selcliente
uo_seltiposag uo_seltiposag
end type
global w_info_archivo_multipuerto w_info_archivo_multipuerto

type variables
Integer	ii_control
String		is_tipoplanilla



end variables

forward prototypes
public function boolean existeplanilla (long al_planilla)
public subroutine revisa_datos ()
end prototypes

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean existeplanilla (long al_planilla);Boolean	lb_Retorno = True
Date		ld_fecha

ii_Control = 0

SELECT Min(defe_fecdes)
	INTO	:ld_fecha
	FROM	dbo.despafrigoen 
	WHERE	plde_codigo =	:uo_SelPlantas.Codigo
	AND	clie_codigo	=	:uo_SelCliente.Codigo
	AND	defe_plasag	=	:al_planilla 
	AND   defe_nturno =  :is_tipoplanilla;
			
If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
	lb_Retorno = False
ElseIf sqlca.SQLCode = 100 Or IsNull(ld_fecha) Then
	MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", Exclamation!, Ok!)
	lb_Retorno = False
 End If

Return lb_Retorno
end function

public subroutine revisa_datos ();long	ll_fila
Integer	li_cont
String	ls_mensaje

FOR ll_fila = 1 TO dw_3.RowCount()
	IF Isnull(dw_3.Object.plde_codmul[ll_fila]) OR dw_3.Object.plde_codmul[ll_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Multipuerto para Planta "+String(dw_3.Object.plde_codigo[ll_fila])
	END IF
	
	IF Isnull(dw_3.Object.dest_codmul[ll_fila]) OR dw_3.Object.dest_codmul[ll_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Multipuerto para Destino "+String(dw_3.Object.dest_codigo[ll_fila])
	END IF
	
	IF Isnull(dw_3.Object.clie_codmul[ll_fila]) OR dw_3.Object.clie_codmul[ll_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Multipuerto para Exportador "+String(dw_3.Object.clie_codigo[ll_fila])
	END IF
	
	IF Isnull(dw_3.Object.embc_codmul[ll_fila]) OR dw_3.Object.embc_codmul[ll_fila]= 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Multipuerto para Agente "+String(dw_3.Object.embc_codigo[ll_fila])
	END IF
	
	IF Isnull(dw_3.Object.espe_codmul[ll_fila]) OR dw_3.Object.espe_codmul[ll_fila]= 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Multipuerto para Especie "+String(dw_3.Object.espe_codigo[ll_fila])
	END IF
	
	IF Isnull(dw_3.Object.puer_codmul[ll_fila]) OR dw_3.Object.puer_codmul[ll_fila]= 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Multipuerto para Puerto "+String(dw_3.Object.puer_codigo[ll_fila])
	END IF
	
	IF Isnull(dw_3.Object.tien_codmul[ll_fila]) OR dw_3.Object.tien_codmul[ll_fila]= 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Multipuerto para Tipo Envase "+String(dw_3.Object.enva_tipoen[ll_fila])
	END IF
	
	IF Isnull(dw_3.Object.vari_codmul[ll_fila]) OR dw_3.Object.vari_codmul[ll_fila]= '' THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Multipuerto para Variedad "+String(dw_3.Object.vari_codigo[ll_fila])
	END IF
	
	IF Isnull(dw_3.Object.tica_codmul[ll_fila]) OR dw_3.Object.tica_codmul[ll_fila]= '' THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Multipuerto para Tipo Camion "+String(dw_3.Object.tica_codigo[ll_fila])
	END IF
NEXT	

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta los Siguientes Registros :" + ls_mensaje + ".", StopSign!, Ok!)
	ii_control = 0
	pb_acepta.Enabled = False
ELSE	
	pb_acepta.Enabled = True
	ii_control = 1
END IF
end subroutine

on w_info_archivo_multipuerto.create
int iCurrent
call super::create
this.st_7=create st_7
this.em_fecha=create em_fecha
this.st_4=create st_4
this.st_3=create st_3
this.em_planilla=create em_planilla
this.st_2=create st_2
this.sle_mensa=create sle_mensa
this.st_1=create st_1
this.st_6=create st_6
this.dw_1=create dw_1
this.cb_valida=create cb_valida
this.dw_3=create dw_3
this.st_5=create st_5
this.st_8=create st_8
this.ddlb_1=create ddlb_1
this.uo_selplantas=create uo_selplantas
this.uo_selcliente=create uo_selcliente
this.uo_seltiposag=create uo_seltiposag
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_7
this.Control[iCurrent+2]=this.em_fecha
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.em_planilla
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.sle_mensa
this.Control[iCurrent+8]=this.st_1
this.Control[iCurrent+9]=this.st_6
this.Control[iCurrent+10]=this.dw_1
this.Control[iCurrent+11]=this.cb_valida
this.Control[iCurrent+12]=this.dw_3
this.Control[iCurrent+13]=this.st_5
this.Control[iCurrent+14]=this.st_8
this.Control[iCurrent+15]=this.ddlb_1
this.Control[iCurrent+16]=this.uo_selplantas
this.Control[iCurrent+17]=this.uo_selcliente
this.Control[iCurrent+18]=this.uo_seltiposag
end on

on w_info_archivo_multipuerto.destroy
call super::destroy
destroy(this.st_7)
destroy(this.em_fecha)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.em_planilla)
destroy(this.st_2)
destroy(this.sle_mensa)
destroy(this.st_1)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.cb_valida)
destroy(this.dw_3)
destroy(this.st_5)
destroy(this.st_8)
destroy(this.ddlb_1)
destroy(this.uo_selplantas)
destroy(this.uo_selcliente)
destroy(this.uo_seltiposag)
end on

event open;call super::open;Boolean lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelTipoSAG.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelTipoSAG.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_codexport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
	uo_SelTipoSAG.Inicia(4)
	
	is_tipoplanilla 				=  '1'
End If
end event

event resize;call super::resize;cb_valida.x = pb_Acepta.x
cb_valida.y = (pb_Acepta.y - cb_valida.Height) - 20
end event

type pb_excel from w_para_informes`pb_excel within w_info_archivo_multipuerto
end type

type st_computador from w_para_informes`st_computador within w_info_archivo_multipuerto
end type

type st_usuario from w_para_informes`st_usuario within w_info_archivo_multipuerto
end type

type st_temporada from w_para_informes`st_temporada within w_info_archivo_multipuerto
end type

type p_logo from w_para_informes`p_logo within w_info_archivo_multipuerto
end type

type st_titulo from w_para_informes`st_titulo within w_info_archivo_multipuerto
integer width = 1815
string text = "Genera Archivo Plano Multipuerto"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_archivo_multipuerto
string tag = "Genera Archivo"
integer x = 2190
integer y = 748
integer taborder = 110
fontcharset fontcharset = ansi!
boolean underline = true
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
long backcolor = 553648127
end type

event pb_acepta::clicked;Long	  	ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_FilasPallets, li_PldSag, ll_Largo
String		lt_fecha, ls_fechas,ls_Archivo, ls_Registro, ls_Archivofin, ls_Texto, ls_textoarchivo
Integer	li_mensaje,li_Archivo, li_Archivofin, li_Lectura


If ii_control = 0 Then
	MessageBox( "Atención", "Debe validar información antes de generar archivo.", StopSign!, Ok!)
	Return	
End If	

If Not DirectoryExists(gs_disco+":\GeneradosMultiPuerto") Then
	MessageBox( "Error...", "El Directorio (" + gs_disco+":\GeneradosMultiPuerto"+ ") no existe  favor revisar ruta para continuar.", StopSign!, Ok!)
	Return
End If

dw_1.reset()
dw_1.SetTransObject(Sqlca)
dw_1.Reset()

sle_mensa.text	= "Recopilando Información"

ll_Numero	= 	Long(em_planilla.text)
li_PldSag	=	uo_SelPlantas.CodigoMulti

ll_Filas	= 	dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo, Dec(ll_Numero),uo_SelTipoSAG.Codigo, is_tipoplanilla)

If ll_Filas = -1 Then
	F_ErrorBaseDatos(sqlca,"Recuperación datos de Planilla S.A.G.")
	Return
ElseIf ll_Filas = 0 Then
	MessageBox("Atención", "No hay información para Planilla Indicada.~r~rIngrese otro Número.", Exclamation!, Ok!)
	pb_acepta.Enabled	= False
	em_planilla.SetFocus()
	Return
End If

ls_Archivo		= 'paso'+String(ll_Numero) + ".TXT"
ls_Archivofin	= uo_SelPlantas.Region +'-'+ String(li_PldSag)+'-'+String(ll_Numero) + ".TXT"

If dw_1.SaveAs(gs_disco+":\GeneradosMultiPuerto\" + ls_Archivo, Text!, False) = -1 Then
	MessageBox("Atención","No se pudo generar el archivo "+ls_Archivo)
	sle_mensa.text	= "Archivo " + ls_Archivo + " No generado"
	Return
Else
 	ll_Largo = FileLength(gs_disco+":\GeneradosMultiPuerto\" + ls_Archivo)
 
 	If ll_Largo = -1 Then 
		MessageBox("Generación de Archivo","No se pudo abrir el Archivo " + gs_disco+":\GeneradosMultiPuerto\" + ls_Archivo + " Generado.")
	  	Return
 	Else                                      

	  	li_Archivo	= FileOpen(gs_disco+":\GeneradosMultiPuerto\" + ls_Archivo, StreamMode!,Read!,LockRead!)
	  	li_Archivofin	= FileOpen(gs_disco+":\GeneradosMultiPuerto\" + ls_Archivofin, StreamMode!,Write!,LockReadWrite!,Replace!)

	  	li_Lectura	= FileRead(li_Archivo, ls_textoarchivo)
	  	ls_Texto    	= String(ls_textoarchivo)
	  	ls_Texto 		= Mid ( ls_textoarchivo, 1, Long(li_Lectura) -2 )
	  
		FileWrite(li_archivofin, ls_Texto)
	  
	  	FileClose(li_Archivo)
	  	FileClose(li_Archivofin)
	  
	  	FileDelete(gs_disco+":\GeneradosMultiPuerto\" + ls_Archivo)
	  	MessageBox("Generación de Archivo","Archivo " + gs_disco+":\GeneradosMultiPuerto\" + ls_ArchivoFin + " Generado.")
		sle_mensa.Text = "Archivo " + gs_disco+":\GeneradosMultiPuerto\" + ls_ArchivoFin + " Generado."
 	End If
 End If

em_planilla.SetFocus()		
end event

type pb_salir from w_para_informes`pb_salir within w_info_archivo_multipuerto
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2190
integer y = 1028
integer taborder = 120
string powertiptext = "Salir [Cerrar Ventana Activa]"
long backcolor = 553648127
end type

type st_7 from statictext within w_info_archivo_multipuerto
integer x = 827
integer y = 1012
integer width = 590
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
string text = "Planilla MultiPuerto"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_info_archivo_multipuerto
integer x = 1307
integer y = 840
integer width = 402
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_4 from statictext within w_info_archivo_multipuerto
integer x = 347
integer y = 616
integer width = 229
integer height = 64
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_archivo_multipuerto
integer x = 347
integer y = 496
integer width = 229
integer height = 64
integer taborder = 20
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

type em_planilla from editmask within w_info_archivo_multipuerto
event getfocus pbm_ensetfocus
integer x = 827
integer y = 840
integer width = 443
integer height = 88
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

event modified;If This.Text = '' Or IsNull(This.Text) Then Return 

IF Not ExistePlanilla(Long(This.Text)) THEN
	This.Text = ''
	This.SetFocus()
Else
	pb_Acepta.Enabled	= True
END IF
end event

type st_2 from statictext within w_info_archivo_multipuerto
integer x = 347
integer y = 852
integer width = 471
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
string text = "Planilla S.A.G."
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_info_archivo_multipuerto
integer x = 288
integer y = 1296
integer width = 1746
integer height = 92
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 65535
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_info_archivo_multipuerto
integer x = 251
integer y = 440
integer width = 1815
integer height = 540
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_archivo_multipuerto
integer x = 251
integer y = 1228
integer width = 1815
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_info_archivo_multipuerto
boolean visible = false
integer x = 2080
integer y = 1472
integer width = 261
integer height = 200
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_despacho_edit_sag"
end type

type cb_valida from commandbutton within w_info_archivo_multipuerto
string tag = "Revisión Datos"
integer x = 2190
integer y = 488
integer width = 293
integer height = 112
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Valida"
end type

event clicked;Long	  	ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_FilasPallets, li_PldSag
String		t_fecha, ls_operacion, ls_fechas,ls_Archivo, ls_Registro, ls_region
Date 		ld_fechaZarpe
Integer	li_mensaje,li_camiones

ii_control = 0

dw_3.SetTransObject(sqlca)

SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME REVISION GENERA ARCHIVO MULTIPUERTO'
sle_mensa.text	= "Recopilando Información"
ll_Numero	= 	Long(em_planilla.text)		
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_archivomultipuerto_valida"
vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo, Long(ll_Numero),uo_SelTipoSAG.Codigo,is_tipoplanilla)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	dw_3.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo, Long(ll_Numero),uo_SelTipoSAG.Codigo,is_tipoplanilla)
	Revisa_Datos()
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF
end event

type dw_3 from datawindow within w_info_archivo_multipuerto
boolean visible = false
integer x = 2359
integer y = 1468
integer width = 293
integer height = 216
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_archivomultipuerto_valida"
end type

type st_5 from statictext within w_info_archivo_multipuerto
integer x = 251
integer y = 980
integer width = 1815
integer height = 248
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_archivo_multipuerto
integer x = 347
integer y = 728
integer width = 229
integer height = 64
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
string text = "Tipo"
boolean focusrectangle = false
end type

type ddlb_1 from dropdownlistbox within w_info_archivo_multipuerto
integer x = 581
integer y = 720
integer width = 1403
integer height = 512
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
string item[] = {"1. Productos Agríc. de Export. Certificados","2. Productos Agr.Export. Cert. (USDA)","3. Fruta a ser Fumigada en U.S.A.","4. Fumigados","5. Fruta a ser Fumigada en México"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;is_tipoplanilla	=	String(index)
end event

type uo_selplantas from uo_seleccion_plantas within w_info_archivo_multipuerto
event destroy ( )
integer x = 585
integer y = 604
integer height = 92
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_info_archivo_multipuerto
event destroy ( )
integer x = 585
integer y = 484
integer height = 92
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_seltiposag from uo_seleccion_tiposag within w_info_archivo_multipuerto
integer x = 713
integer y = 1092
integer height = 84
integer taborder = 130
boolean bringtotop = true
end type

on uo_seltiposag.destroy
call uo_seleccion_tiposag::destroy
end on

