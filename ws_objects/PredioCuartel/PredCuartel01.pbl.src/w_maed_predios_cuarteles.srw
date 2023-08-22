$PBExportHeader$w_maed_predios_cuarteles.srw
forward
global type w_maed_predios_cuarteles from w_mant_encab_deta
end type
type tab_1 from tab within w_maed_predios_cuarteles
end type
type tabpage_1 from userobject within tab_1
end type
type dw_general from datawindow within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_general dw_general
end type
type tabpage_3 from userobject within tab_1
end type
type dw_legal from datawindow within tabpage_3
end type
type tabpage_3 from userobject within tab_1
dw_legal dw_legal
end type
type tabpage_2 from userobject within tab_1
end type
type dw_observacion from datawindow within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_observacion dw_observacion
end type
type tab_1 from tab within w_maed_predios_cuarteles
tabpage_1 tabpage_1
tabpage_3 tabpage_3
tabpage_2 tabpage_2
end type
type dw_3 from uo_dw within w_maed_predios_cuarteles
end type
end forward

global type w_maed_predios_cuarteles from w_mant_encab_deta
integer width = 3397
integer height = 2428
string title = "PREDIOS / CUARTELES"
string menuname = ""
event ue_imprimir ( )
tab_1 tab_1
dw_3 dw_3
end type
global w_maed_predios_cuarteles w_maed_predios_cuarteles

type variables
w_mant_deta_cuarteles 	iw_mantencion
String						is_Rut, is_codigo
Integer 					ii_sw

Boolean					ib_nivelvalid

uo_Zonas				iuo_Zonas
uo_agronomo 			iuo_agronomo
uo_provincias  			iuo_provincias
uo_comunasexp  		iuo_comunasexp
uo_productores 		iuo_productores

uo_grabatablas			iuo_grabatablas

DataWindowChild  	dw_zona, idwc_agronomos, idwc_comunas, idwc_Provincias, idwc_regiones, idwc_Especies, idwc_Variedades
end variables

forward prototypes
public subroutine habilitaingreso (string columna)
public subroutine habilitaencab (boolean habilita)
public subroutine existepredio (string columna, integer tipo)
public subroutine buscaproductor ()
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine wf_replicacion ()
public subroutine wf_filtro (string filtro, ref datawindow adw)
protected function integer wf_modifica ()
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE PREDIOS Y CUARTELES"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_informe_predcuarteles"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(-1, dw_2.Object.prod_codigo[1], -1, -1, -1, -1, dw_2.Object.prpr_codigo[1])

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.t_22.Visible = False
	vinf.dw_1.Modify('DataWindow.Zoom = 50')

	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

public subroutine habilitaingreso (string columna);Boolean	lb_estado = True

IF columna <> "prod_codigo" AND &
	(dw_2.GetItemNumber(1, "prod_codigo") = 0 OR IsNull(dw_2.GetItemNumber(1, "prod_codigo"))) THEN
	lb_estado = False
END IF

IF columna <> "prpr_codigo" AND &
	(dw_2.GetItemNumber(1, "prpr_codigo") = 0 OR IsNull(dw_2.GetItemNumber(1, "prpr_codigo"))) THEN
	lb_estado = False
END IF

IF columna <> "prpr_nombre" AND &
	(dw_2.GetItemString(1, "prpr_nombre") = "" OR IsNull(dw_2.GetItemString(1, "prpr_nombre"))) THEN
	lb_estado = False
END IF

IF ib_nivelvalid THEN
	IF columna <> "prpr_direcc" AND &
		(dw_2.GetItemString(1, "prpr_direcc") = "" OR IsNull(dw_2.GetItemString(1, "prpr_direcc"))) THEN
		lb_estado = False
	END IF
	
	IF columna <> "regi_codigo" AND &
		(dw_2.GetItemNumber(1, "regi_codigo") = 0 OR IsNull(dw_2.GetItemNumber(1, "regi_codigo"))) THEN
		lb_estado = False
	END IF
	
	IF columna <> "prov_codigo" AND &
		(dw_2.GetItemNumber(1, "prov_codigo") = 0 OR IsNull(dw_2.GetItemNumber(1, "prov_codigo"))) THEN
		lb_estado = False
	END IF
	
	IF columna <> "comu_codigo" AND &
		(dw_2.GetItemNumber(1, "comu_codigo") = 0 OR IsNull(dw_2.GetItemNumber(1, "comu_codigo"))) THEN
		lb_estado = False
	END IF
	
	IF columna <> "zona_codigo" AND &
		(dw_2.GetItemNumber(1, "zona_codigo") = 0 OR IsNull(dw_2.GetItemNumber(1, "zona_codigo"))) THEN
		lb_estado = False
	END IF
ELSE
	IF NOT pb_eliminar.Enabled AND lb_estado THEN
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	END IF
END IF

pb_ins_det.Enabled = lb_estado
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	Tab_1.TabPage_1.dw_general.Object.prod_codigo.Protect	= 0
	Tab_1.TabPage_1.dw_general.Object.prpr_codigo.Protect	= 0
	Tab_1.TabPage_1.dw_general.Object.prpr_nombre.Protect	= 0
	Tab_1.TabPage_1.dw_general.Object.prpr_prepro.Protect	= 0
	Tab_1.TabPage_1.dw_general.Object.prpr_nompro.Protect	= 0
	Tab_1.TabPage_1.dw_general.Object.regi_codigo.Protect	= 0
	Tab_1.TabPage_1.dw_general.Object.prov_codigo.Protect	= 0
	Tab_1.TabPage_1.dw_general.Object.comu_codigo.Protect	= 0
	Tab_1.TabPage_1.dw_general.Object.zona_codigo.Protect	= 0
	
	Tab_1.TabPage_1.dw_general.Object.prod_codigo.Color	= 0
	Tab_1.TabPage_1.dw_general.Object.prpr_codigo.Color		= 0
	Tab_1.TabPage_1.dw_general.Object.prod_nombre.Color	= 0
	Tab_1.TabPage_1.dw_general.Object.prpr_nombre.Color	= 0
	Tab_1.TabPage_1.dw_general.Object.prpr_nompro.Color	= 0
	Tab_1.TabPage_1.dw_general.Object.prpr_prepro.Color		= 0
	Tab_1.TabPage_1.dw_general.Object.regi_codigo.Color		= 0
	Tab_1.TabPage_1.dw_general.Object.prov_codigo.Color		= 0
	Tab_1.TabPage_1.dw_general.Object.comu_codigo.Color	= 0
	Tab_1.TabPage_1.dw_general.Object.zona_codigo.Color	= 0
	
	
	   Tab_1.TabPage_1.dw_general.Object.prod_codigo.BackGround.Color		= Rgb(255,255,255)
	Tab_1.TabPage_1.dw_general.Object.prpr_codigo.BackGround.Color		= Rgb(255,255,255)
	Tab_1.TabPage_1.dw_general.Object.prod_nombre.BackGround.Color	= Rgb(255,255,255)
	Tab_1.TabPage_1.dw_general.Object.prpr_nombre.BackGround.Color	= Rgb(255,255,255)
	Tab_1.TabPage_1.dw_general.Object.prpr_nompro.BackGround.Color	= Rgb(255,255,255)
	Tab_1.TabPage_1.dw_general.Object.prpr_prepro.BackGround.Color		= Rgb(255,255,255)
	Tab_1.TabPage_1.dw_general.Object.regi_codigo.BackGround.Color		= Rgb(255,255,255)
	Tab_1.TabPage_1.dw_general.Object.prov_codigo.BackGround.Color		= Rgb(255,255,255)
	Tab_1.TabPage_1.dw_general.Object.comu_codigo.BackGround.Color	= Rgb(255,255,255)
	Tab_1.TabPage_1.dw_general.Object.zona_codigo.BackGround.Color	= Rgb(255,255,255)
	
	Tab_1.TabPage_1.dw_general.setcolumn("prod_codigo")
	
ELSE
		Tab_1.TabPage_1.dw_general.Object.prod_codigo.Protect	= 1
		Tab_1.TabPage_1.dw_general.Object.prpr_codigo.Protect	= 1
		Tab_1.TabPage_1.dw_general.Object.prod_codigo.Color	= Rgb(255,255,255)
		Tab_1.TabPage_1.dw_general.Object.prpr_codigo.Color		= Rgb(255,255,255)
		Tab_1.TabPage_1.dw_general.Object.prod_codigo.BackGround.Color	= 553648127
		Tab_1.TabPage_1.dw_general.Object.prpr_codigo.BackGround.Color	= 553648127
		
	IF Upper(gstr_us.Nombre) <> Upper(gstr_parametros.admin1) AND Upper(gstr_us.Nombre) <> Upper(gstr_parametros.admin2) THEN		
		Tab_1.TabPage_1.dw_general.Object.prpr_nombre.Protect	= 1
		Tab_1.TabPage_1.dw_general.Object.prpr_nompro.Protect	= 1
		Tab_1.TabPage_1.dw_general.Object.prpr_prepro.Protect	= 1
		Tab_1.TabPage_1.dw_general.Object.regi_codigo.Protect	= 1
		Tab_1.TabPage_1.dw_general.Object.prov_codigo.Protect	= 1
		Tab_1.TabPage_1.dw_general.Object.comu_codigo.Protect	= 1
		Tab_1.TabPage_1.dw_general.Object.zona_codigo.Protect	= 1
		
		Tab_1.TabPage_1.dw_general.Object.prod_nombre.Color	= Rgb(255,255,255)
		Tab_1.TabPage_1.dw_general.Object.prpr_nombre.Color	= Rgb(255,255,255)
		Tab_1.TabPage_1.dw_general.Object.prpr_nompro.Color	= Rgb(255,255,255)
		Tab_1.TabPage_1.dw_general.Object.prpr_prepro.Color		= Rgb(255,255,255)
		Tab_1.TabPage_1.dw_general.Object.regi_codigo.Color		= Rgb(255,255,255)
		Tab_1.TabPage_1.dw_general.Object.prov_codigo.Color		= Rgb(255,255,255)
		Tab_1.TabPage_1.dw_general.Object.comu_codigo.Color	= Rgb(255,255,255)
		Tab_1.TabPage_1.dw_general.Object.zona_codigo.Color	= Rgb(255,255,255)
		
		Tab_1.TabPage_1.dw_general.Object.prod_nombre.BackGround.Color	= 553648127
		Tab_1.TabPage_1.dw_general.Object.prpr_nombre.BackGround.Color	= 553648127
		Tab_1.TabPage_1.dw_general.Object.prpr_nompro.BackGround.Color	= 553648127
		Tab_1.TabPage_1.dw_general.Object.prpr_prepro.BackGround.Color		= 553648127
		Tab_1.TabPage_1.dw_general.Object.regi_codigo.BackGround.Color		= 553648127
		Tab_1.TabPage_1.dw_general.Object.prov_codigo.BackGround.Color		= 553648127
		Tab_1.TabPage_1.dw_general.Object.comu_codigo.BackGround.Color	= 553648127
		Tab_1.TabPage_1.dw_general.Object.zona_codigo.BackGround.Color	= 553648127
	END IF	
END IF

end subroutine

public subroutine existepredio (string columna, integer tipo);Long		ll_Productor
Integer	li_Predio

ll_Productor	=	tab_1.tabpage_1.dw_general.GetItemNumber(1,"prod_codigo")
li_Predio	 	=	tab_1.tabpage_1.dw_general.GetItemNumber(1,"prpr_codigo")

CHOOSE CASE tipo
	CASE 1
		li_Predio	= Integer(columna)	
	CASE 2
		ll_Productor	= Long(columna)	
		
END CHOOSE

SELECT	prpr_nombre 
	INTO	:istr_mant.argumento[3]
	FROM	dbo.spro_prodpredio
	WHERE	prpr_codigo = :li_Predio
	AND	prod_codigo = :ll_Productor;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura Tabla Predios")
ELSEIF sqlca.SQLCode = 0 THEN
	This.TriggerEvent("ue_recuperadatos")
END IF

RETURN
end subroutine

public subroutine buscaproductor (); istr_busq.Argum[1] = '-1'
OpenWithParm(w_busc_productores, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[3] = "" THEN
	tab_1.tabpage_1.dw_general.SetColumn("prod_codigo")
	tab_1.tabpage_1.dw_general.SetFocus()
ELSE
	tab_1.tabpage_1.dw_general.SetItem(il_fila,"prod_codigo",Long(istr_busq.argum[1]))
	tab_1.tabpage_1.dw_general.Object.prod_nombre[il_Fila]=  istr_busq.argum[2]
	istr_mant.argumento[2] = istr_busq.argum[1] // istr_busq.argum[2]		JISG 02/07/2014
	dw_1.SetFocus()
END IF

RETURN

end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora
dw_2.GrupoFecha	=	ldt_FechaHora
dw_3.GrupoFecha	=	ldt_FechaHora

If Not dw_2.uf_check_required(0) Then RETURN False

If Not dw_1.uf_validate(0) Then RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

wf_replicacion()

If Borrando Then
	If dw_3.Update(True, False) = 1 Then
		If dw_1.Update(True, False) = 1 Then
			If dw_2.Update(True, False) = 1 Then
				Commit;
				
				If sqlca.SQLCode <> 0 Then
					F_ErrorBaseDatos(sqlca, This.Title)
					
					RollBack;
				Else
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
					dw_3.ResetUpdate()
				End If
			Else
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			End If
				Else
			F_ErrorBaseDatos(sqlca, This.Title)
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
	End If
Else
	If dw_2.Update(True, False) = 1 Then
		If dw_1.Update(True, False) = 1 Then
			If dw_3.Update(True, False) = 1 Then
				Commit;
				
				If sqlca.SQLCode <> 0 Then
					F_ErrorBaseDatos(sqlca, This.Title)
					
					RollBack;
				Else
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
					dw_3.ResetUpdate()
				End If
			Else
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
	End If
End If

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine wf_replicacion ();Integer	ll_Fila, ll_Fila2, ll_Fila3, li_Correo
uo_Mail			iuo_Mail
dwItemStatus	lis_Error

String				ls_Asunto = 'Creacion de Predios: ' , ls_texto = 'Estimados:~n~n', ls_Correo[]

ll_Fila		= 	dw_1.GetNextModified(ll_Fila, Primary!)
ll_Fila2	= 	dw_2.GetNextModified(ll_Fila2, Primary!)
ll_Fila3	=	dw_3.GetNextModified(ll_Fila3, Primary!)

wf_Filtro("" , dw_3)

iuo_Mail	=	Create uo_Mail

If ll_fila > 0 OR ll_fila2 > 0 Then
	If iuo_grabatablas.existereplicatablas(gi_CodExport)  Then
		iuo_grabatablas.replicatabla_prodpredio(dw_2)
		iuo_grabatablas.replicatabla_prodcuartel(dw_1)
		iuo_grabatablas.replicatabla_cuartelvariedad(dw_3)
	End If
	
	If Not IsNull(gstr_Parempresa.Correo_Soporte_Zonal) And gstr_Parempresa.Correo_Soporte_Zonal <> '' Then
		For ll_Fila = 1 To dw_2.RowCount()
			lis_Error = dw_2.GetItemStatus(ll_Fila, 0, Primary!)
			
			If   lis_Error = NewModified! Or  lis_Error  = DataModified! Then
				ls_texto += 'Predio: ' + String(dw_2.Object.prpr_codigo[ll_Fila], '0000') + ', para Productor: ' + String(dw_1.Object.prod_codigo[ll_Fila], '00000') + &
								', en Planta: ' + String(gi_CodPlanta, '0000') + '.~n'
				ls_Asunto +=  String(dw_2.Object.prpr_codigo[ll_Fila], '0000') + ' / ' 
			End If
		Next
		
		ls_Texto +=  '~n~n~nAtte.~nSoporte TI, RIOblanco SPA.'
		ls_Correo = {gstr_Parempresa.Correo_Soporte_Zonal}
		iuo_mail.of_Send(ls_Correo,ls_asunto,ls_texto,0)
							
	End If	
	
End If

Destroy iuo_Mail
end subroutine

public subroutine wf_filtro (string filtro, ref datawindow adw);adw.SetFilter(Filtro)
adw.Filter()
end subroutine

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF dw_3.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0
IF dw_3.ModifiedCount() > 0 THEN RETURN 0

RETURN 1
end function

event open;dw_2.ShareData(Tab_1.TabPage_1.dw_general)
dw_2.ShareData(Tab_1.TabPage_2.dw_observacion)
dw_2.ShareData(Tab_1.TabPage_3.dw_legal)

Tab_1.TabPage_1.dw_general.SetTransObject(SQLCA)
Tab_1.TabPage_2.dw_observacion.SetTransObject(SQLCA)
Tab_1.TabPage_3.dw_legal.SetTransObject(SQLCA)

dw_3.SetTransObject(SqlCa)
istr_mant.dw2		=	dw_3

buscar	= "Código:Nprcc_codigo,Descripción:Sprcc_nombre"
ordenar	= "Código:prcc_codigo,Descripción:prcc_nombre"

iuo_Zonas			=	Create	uo_Zonas
iuo_agronomo		=  Create  	uo_agronomo
iuo_provincias 		=	Create	uo_provincias
iuo_comunasexp 	=  Create   	uo_comunasexp
iuo_productores 	=	Create   	uo_productores

iuo_grabatablas	=	Create	uo_grabatablas

Tab_1.TabPage_1.dw_general.GetChild("zona_codigo", dw_zona)
dw_zona.SetTransObject(sqlca)
dw_zona.Retrieve()

Tab_1.TabPage_3.dw_legal.GetChild("ccag_codigo", idwc_agronomos)
idwc_agronomos.SetTransObject(sqlca)
idwc_agronomos.Retrieve(0)
idwc_agronomos.SetSort("ccag_nombre A")
idwc_agronomos.Sort()

Tab_1.TabPage_1.dw_general.GetChild("regi_codigo", idwc_regiones)
idwc_regiones.SetTransObject(sqlca)
idwc_regiones.Retrieve(0)

Tab_1.TabPage_1.dw_general.GetChild("prov_codigo", idwc_provincias)
idwc_provincias.SetTransObject(sqlca)
idwc_provincias.Retrieve(0)

Tab_1.TabPage_1.dw_general.GetChild("comu_codigo", idwc_comunas)
idwc_comunas.SetTransObject(sqlca)
idwc_comunas.Retrieve(0,0)

dw_2.GetChild("comu_codigo", idwc_comunas)
idwc_comunas.SetTransObject(sqlca)
idwc_comunas.Retrieve(0,0)
idwc_comunas.InsertRow(0)

dw_2.GetChild("zona_codigo", dw_zona)
dw_zona.SetTransObject(sqlca)
dw_zona.Retrieve()

dw_2.GetChild("ccag_codigo", idwc_agronomos)
idwc_agronomos.SetTransObject(sqlca)
idwc_agronomos.Retrieve(0)
idwc_agronomos.SetSort("ccag_nombre A")
idwc_agronomos.Sort()
idwc_agronomos.InsertRow(0)

dw_2.GetChild("regi_codigo", idwc_regiones)
idwc_regiones.SetTransObject(sqlca)
idwc_regiones.Retrieve(0)
idwc_regiones.InsertRow(0)

dw_2.GetChild("prov_codigo", idwc_provincias)
idwc_provincias.SetTransObject(sqlca)
idwc_provincias.Retrieve(0)
idwc_provincias.InsertRow(0)

Call Super::Open
end event

event ue_borra_detalle;call super::ue_borra_detalle;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

wf_Filtro("prcc_codigo = " + String(dw_1.Object.prcc_codigo[il_Fila]), dw_3)

OpenWithParm(iw_mantencion, istr_mant)

wf_Filtro("", dw_3)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_1.RowCount() = 0 THEN 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True

If dw_3.RowCount() > 0 And dw_1.RowCount() > 0  Then wf_Filtro("prcc_codigo = " + String(dw_1.Object.prcc_codigo[il_Fila]), dw_3)

OpenWithParm(iw_mantencion, istr_mant)

wf_Filtro("", dw_3)

IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = False THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_dd, ll_fila_d, ll_fila_e, respuesta

dw_3.GetChild("espe_codigo", idwc_Especies)
idwc_Especies.SetTransObject(sqlca)
idwc_Especies.Retrieve()

dw_3.GetChild("vari_codigo", idwc_Variedades)
idwc_Variedades.SetTransObject(sqlca)
idwc_Variedades.Retrieve(-1)

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]))
	ll_fila_dd	= dw_3.Retrieve(Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]), -1)
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		tab_1.tabpage_1.dw_general.GetChild("prov_codigo", idwc_provincias)
		idwc_provincias.SetTransObject(sqlca)
		idwc_provincias.Retrieve(dw_2.Object.regi_codigo[1])
		tab_1.tabpage_1.dw_general.GetChild("comu_codigo", idwc_comunas)
		idwc_comunas.SetTransObject(sqlca)
		idwc_comunas.Retrieve(dw_2.Object.regi_codigo[1],dw_2.Object.prov_codigo[1])
		idwc_comunas.InsertRow(0)
		tab_1.tabpage_3.dw_legal.GetChild("ccag_codigo", idwc_agronomos)
		idwc_agronomos.SetTransObject(sqlca)
		idwc_agronomos.Retrieve(tab_1.tabpage_1.dw_general.Object.zona_codigo[1])
		
		DO
			ll_fila_d	= dw_1.Retrieve(Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_imprimir.Enabled	= True
				pb_ins_det.Enabled	= True

				IF ll_fila_d > 0 THEN
					pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
				ELSE
					pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_predios_cuarteles.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.dw_3
end on

on w_maed_predios_cuarteles.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.dw_3)
end on

event ue_seleccion;call super::ue_seleccion;str_busqueda	lstr_busq

lstr_busq.Argum[1]	=	""
IF IsNull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0 THEN
	lstr_busq.Argum[2] = '0'
ELSE
	lstr_busq.Argum[2] = String(tab_1.tabpage_1.dw_general.Object.prod_codigo[1])
END IF

OpenWithParm(w_busc_prodpredio,lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_mant.argumento[1] = lstr_busq.argum[1]
	istr_mant.argumento[2] = lstr_busq.argum[2]
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_nuevo;Long		ll_modif1, ll_modif2, ll_modif3

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
			ll_modif3	=	dw_3.GetNextModified(0, Primary!)
		
//			IF ib_ModEncab OR dw_1.GetNextModified(0, Primary!) > 0 THEN
			IF dw_1.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()
dw_3.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()		

HabilitaEncab(True)
end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	
	wf_Filtro("prcc_codigo = " + String(dw_1.Object.prcc_codigo[il_Fila]), dw_3)
	
	OpenWithParm(iw_mantencion, istr_mant)
	
	wf_Filtro("", dw_3)
	
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila
Integer  li_cont
String	ls_Mensaje, ls_colu[],ls_Null

SetNull(ls_Null)

IF dw_2.RowCount() > 0 THEN	
	IF Isnull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Productor"
		ls_colu[li_cont]	= "prod_codigo"
	END IF	
	
	IF Isnull(dw_2.Object.prpr_codigo[1]) OR dw_2.Object.prpr_codigo[1] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Predio"
		ls_colu[li_cont]	= "prpr_codigo"
	END IF	
	
	IF Isnull(dw_2.Object.prpr_nombre[1]) OR dw_2.Object.prpr_nombre[1] = "" THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nNombre Predio"
		ls_colu[li_cont]	= "prpr_nombre"
	END IF
	
	IF ib_nivelvalid THEN
		IF Isnull(dw_2.Object.prpr_direcc[1]) OR dw_2.Object.prpr_direcc[1] = "" THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nDirección"
			ls_colu[li_cont]	= "prpr_direcc"
		END IF	
		
		IF Isnull(dw_2.Object.regi_codigo[1]) OR dw_2.Object.regi_codigo[1] = 0 THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nRegión"
			ls_colu[li_cont]	= "regi_codigo"
		END IF	
		
		IF Isnull(dw_2.Object.prov_codigo[1]) OR dw_2.Object.prov_codigo[1] = 0 THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nProvincia"
			ls_colu[li_cont]	= "prov_codigo"
		END IF
		
		IF Isnull(dw_2.Object.comu_codigo[1]) OR dw_2.Object.comu_codigo[1] = 0 THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nComuna"
			ls_colu[li_cont]	= "comu_codigo"
		END IF
			
		IF Isnull(dw_2.Object.zona_codigo[1]) OR dw_2.Object.zona_codigo[1] = 0 THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nZona"
			ls_colu[li_cont]	= "zona_codigo"
		END IF		
	END IF
END IF

IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + &
					ls_mensaje + ".", StopSign!, Ok!)
		dw_2.SetColumn(ls_colu[1])
		dw_2.SetFocus()
		HabilitaEncab(TRUE)
		Message.DoubleParm = -1
		RETURN		
ELSE		
	FOR ll_fila = 1 TO dw_1.RowCount()	
		IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! OR &
			dw_1.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
			dw_1.SetItem(ll_fila, "prod_codigo", dw_2.Object.prod_codigo[1])
			dw_1.SetItem(ll_fila, "prpr_codigo", dw_2.Object.prpr_codigo[1])
		END IF	
	NEXT
END IF

HabilitaEncab(FALSE)	
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0, &
			li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 500
	maximo		=	dw_1.width
END IF

IF tab_1.width > il_AnchoDw_1 THEN
	maximo		=	tab_1.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 500
	maximo		=	dw_1.width
END IF

tab_1.x					= 37 + Round((maximo - tab_1.width) / 2, 0)
tab_1.y					= 37

dw_2.x					=	37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					=	tab_1.y + tab_1.Height + 37

dw_1.x					=	37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					=	tab_1.y + tab_1.Height + 37

dw_1.height				=	This.WorkSpaceHeight() - dw_1.y - 41


li_posic_x				=	This.WorkSpaceWidth() - 400
li_posic_y				=	30 // gb_1.y + 68

IF pb_buscar.Visible THEN
	pb_buscar.x				=	li_posic_x
	pb_buscar.y				=	li_posic_y
	pb_buscar.width		=	li_Ancho
	pb_buscar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				=	li_posic_x
	pb_nuevo.y				=	li_posic_y
	pb_nuevo.width			=	li_Ancho
	pb_nuevo.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			=	li_posic_x
	pb_eliminar.y			=	li_posic_y
	pb_eliminar.width		=	li_Ancho
	pb_eliminar.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				=	li_posic_x
	pb_grabar.y				=	li_posic_y
	pb_grabar.width		=	li_Ancho
	pb_grabar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_salir.Visible THEN
	pb_salir.x				=	li_posic_x
	pb_salir.y				=	li_posic_y
	pb_salir.width			=	li_Ancho
	pb_salir.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

pb_ins_det.x			=	li_posic_x
pb_ins_det.y			=	1750
pb_ins_det.width		=	li_Ancho
pb_ins_det.height		=	li_Alto

pb_eli_det.x			=	li_posic_x
pb_eli_det.y			=	pb_ins_det.y + li_Siguiente
pb_eli_det.width		=	li_Ancho
pb_eli_det.height		=	li_Alto
end event

type dw_1 from w_mant_encab_deta`dw_1 within w_maed_predios_cuarteles
event nomover pbm_syscommand
integer x = 59
integer y = 1260
integer width = 2857
integer height = 732
string title = "Detalle de Cuarteles"
string dataobject = "dw_mues_cuarteles"
boolean livescroll = false
end type

event dw_1::nomover;uint wParam, lParam

wParam = Message.WordParm

Choose Case wParam
	Case 61456, 61458
	     Message.Processed = True
	     Message.ReturnValue = 0

End Choose
end event

type dw_2 from w_mant_encab_deta`dw_2 within w_maed_predios_cuarteles
boolean visible = false
integer x = 0
integer y = 0
integer width = 2738
integer height = 940
integer taborder = 80
boolean titlebar = true
string dataobject = "dw_mant_prodpredio"
boolean resizable = true
end type

event dw_2::clicked;//
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_maed_predios_cuarteles
integer x = 3031
integer y = 252
integer taborder = 30
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_maed_predios_cuarteles
integer x = 3031
integer y = 480
integer taborder = 40
end type

type pb_grabar from w_mant_encab_deta`pb_grabar within w_maed_predios_cuarteles
integer x = 3035
integer y = 688
integer taborder = 50
end type

event pb_grabar::clicked;Tab_1.TabPage_1.dw_general.AcceptText()
Tab_1.TabPage_2.dw_observacion.AcceptText()
Tab_1.TabPage_3.dw_legal.AcceptText()
dw_2.AcceptText()
Parent.TriggerEvent("ue_guardar")
end event

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_maed_predios_cuarteles
integer x = 3031
integer taborder = 60
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_maed_predios_cuarteles
integer x = 3031
integer y = 1156
integer taborder = 70
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_maed_predios_cuarteles
integer x = 3035
integer y = 1520
integer taborder = 90
end type

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_maed_predios_cuarteles
integer x = 3035
integer y = 1700
integer taborder = 100
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_maed_predios_cuarteles
integer x = 3031
integer y = 76
integer taborder = 20
end type

type tab_1 from tab within w_maed_predios_cuarteles
integer x = 64
integer width = 2789
integer height = 1196
integer taborder = 10
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_3 tabpage_3
tabpage_2 tabpage_2
end type

on tab_1.create
this.tabpage_1=create tabpage_1
this.tabpage_3=create tabpage_3
this.tabpage_2=create tabpage_2
this.Control[]={this.tabpage_1,&
this.tabpage_3,&
this.tabpage_2}
end on

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_3)
destroy(this.tabpage_2)
end on

event losefocus;Tab_1.TabPage_1.dw_general.AcceptText()
Tab_1.TabPage_2.dw_observacion.AcceptText()
Tab_1.TabPage_3.dw_legal.AcceptText()
dw_2.AcceptText()

end event

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 100
integer width = 2752
integer height = 1080
long backcolor = 16777215
string text = "Antecedentes Generales del Predio"
long tabtextcolor = 33554432
long tabbackcolor = 16777215
long picturemaskcolor = 553648127
dw_general dw_general
end type

on tabpage_1.create
this.dw_general=create dw_general
this.Control[]={this.dw_general}
end on

on tabpage_1.destroy
destroy(this.dw_general)
end on

type dw_general from datawindow within tabpage_1
integer x = 9
integer y = 44
integer width = 2674
integer height = 1008
integer taborder = 30
string title = "none"
string dataobject = "dw_mant_prodpredio_id"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Long		ll_fila, ll_productor
Integer	li_Zona
String		ls_campo, ls_Nula, ls_nombre, columna

ls_campo = dwo.Name

SetNull(ls_Nula)
ll_productor	= tab_1.tabpage_1.dw_general.Object.prod_codigo[Row]
ls_nombre	= tab_1.tabpage_1.dw_general.Object.prod_nombre[Row]

Choose Case ls_campo	
	Case "prod_codigo"
		If Not iuo_productores.existe(Long(data),True,sqlca) Then
			This.SetItem(Row, "prod_codigo", Long(ls_Nula))
			This.SetItem(Row, "prod_nombre", ls_Nula)
			 Return 1
	   Else
			This.Object.prod_nombre[Row]	=	iuo_productores.Nombre
			This.Object.zona_codigo[Row]	=	iuo_productores.Zona
			istr_mant.argumento[2] 			= 	Data
			
			If iuo_productores.Exportador	=	gi_CodExport Then
				ib_nivelvalid	=	True
			Else
				ib_nivelvalid	=	False
			End If
			
			ExistePredio(data,2)
	   End If	
		
	CASE "prpr_idpclb"
		This.Object.prpr_idpclb[Row]	=	Right('00000000', 8 - Len(data)) + Data
		Return 1
	
	Case "prpr_codigo"
		If IsNull(This.Object.prod_codigo[Row]) Or This.Object.prod_codigo[Row] = 0 Then
			MessageBox("Atención", "Debe seleccionar un Productor",Information!)
			This.SetItem(row,"prpr_codigo", Integer(ls_Nula))
			This.SetColumn("prod_codigo")
			Return 1	
		Else
			istr_mant.argumento[1]	=  data
			ExistePredio(data,1)
		End If
		
	Case "zona_codigo"
		If iuo_zonas.existe(Integer(data),True, sqlca) Then
			Tab_1.TabPage_3.dw_legal.SetItem(Row, "ccag_codigo", Integer(ls_Nula))
			Tab_1.TabPage_3.dw_legal.GetChild("ccag_codigo", idwc_agronomos)
			idwc_agronomos.Retrieve(Integer(Data))
			idwc_agronomos.InsertRow(0)
		Else
			This.Setitem(Row, "zona_codigo", Integer(ls_Nula))
			Return 1
		End If		
			
	Case "regi_codigo"
			If Integer(data) <> 0 Then
				This.SetItem(Row, "prov_codigo", Integer(ls_Nula))
				This.SetItem(Row, "comu_codigo", Integer(ls_Nula))
				This.GetChild("prov_codigo", idwc_provincias)
				idwc_provincias.Retrieve(Integer(Data))
			Else
				This.Setitem(Row, "regi_codigo", Integer(ls_Nula))
				Return 1
			End If
			
	Case "prov_codigo"
			If Not iuo_provincias.existe(This.Object.regi_codigo[Row],Integer(data),True,Sqlca) Then
				This.SetItem(Row,"prov_codigo", Integer(ls_Nula))
				This.SetFocus()
				Return 1
			Else
				This.SetItem(Row, "comu_codigo", Integer(ls_Nula))
				This.GetChild("comu_codigo", idwc_comunas)
				IF idwc_comunas.Retrieve(dw_general.Object.regi_codigo[row],Integer(Data)) = 0 THEN 
					MessageBox('Atención', 'No Existe información para los parametros seleccionados', Exclamation!, OK!)
				END IF
					
			End If
			
	Case "prpr_glncod"
		If Len(Data) <> 13 Then
			MessageBox('Alerta', 'Largo de Codigo GLN (Global Location Number) debe ser de largo 13.', Exclamation!, OK!)
			This.SetItem(Row, ls_Campo, ls_Nula)
			Return 1
		End If
			
End Choose

habilitaingreso(ls_Campo)
end event

event itemerror;RETURN 1
end event

event clicked;
CHOOSE CASE dwo.Name
	CASE "buscaproductor"
		buscaproductor()
END CHOOSE


end event

type tabpage_3 from userobject within tab_1
integer x = 18
integer y = 100
integer width = 2752
integer height = 1080
long backcolor = 16777215
string text = "Antecedentes Legales"
long tabtextcolor = 33554432
long tabbackcolor = 30586022
long picturemaskcolor = 536870912
dw_legal dw_legal
end type

on tabpage_3.create
this.dw_legal=create dw_legal
this.Control[]={this.dw_legal}
end on

on tabpage_3.destroy
destroy(this.dw_legal)
end on

type dw_legal from datawindow within tabpage_3
integer x = 9
integer y = 44
integer width = 2688
integer height = 856
integer taborder = 40
string dragicon = "1"
string title = "none"
string dataobject = "dw_mant_prodpredio_legal"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Long		ll_fila, ll_productor
Integer	li_Zona
String	ls_campo, ls_Nula, ls_nombre, columna

ls_campo = GetColumnName()

SetNull(ls_Nula)

CHOOSE CASE ls_campo
		
		CASE "zona_codigo"
			
			IF iuo_zonas.existe(Integer(data),TRUE, sqlca) THEN
				This.GetChild("ccag_codigo", idwc_agronomos)
				idwc_agronomos.Retrieve(Integer(Data))
				idwc_agronomos.InsertRow(0)
				This.SetItem(Row, "ccag_codigo", Integer(ls_Nula))
			ELSE
				This.Setitem(Row, "zona_codigo", Integer(ls_Nula))
				RETURN 1
			END IF
		
		CASE "ccag_codigo"
			
		  IF Not iuo_agronomo.existe(Integer(data),TRUE,sqlca) THEN
			  This.SetItem(Row, "ccag_codigo", Integer(ls_Nula))
			  RETURN 1
	     END IF	
		
		CASE "prpr_rutrep"
			
				IF Len(Data) > 6 THEN
					is_Rut = Fill("0",10 - Len(data)) + data
				ELSE
					is_Rut = Fill("0",6 - Len(data)) + data
				END IF
				
				is_rut = F_verrut(is_Rut, True)
				IF is_rut = ""  THEN
					This.SetItem(row, "prpr_rutrep", ls_Nula)
					RETURN 1
				END IF
				
		CASE "prpr_admini"
			
				IF Len(Data) > 6 THEN
					is_codigo = Fill("0",10 - Len(data)) + data
				ELSE
					is_codigo = Fill("0",6 - Len(data)) + data
				END IF
				
				is_codigo = F_verrut(is_codigo, True)
				IF is_codigo = ""  THEN
					This.SetItem(Row, "prpr_admini", ls_Nula)
					RETURN 1
				END IF			
END CHOOSE

habilitaingreso(ls_Campo)
end event

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 100
integer width = 2752
integer height = 1080
long backcolor = 16777215
string text = "Observaciones"
long tabtextcolor = 33554432
long tabbackcolor = 30586022
long picturemaskcolor = 536870912
dw_observacion dw_observacion
end type

on tabpage_2.create
this.dw_observacion=create dw_observacion
this.Control[]={this.dw_observacion}
end on

on tabpage_2.destroy
destroy(this.dw_observacion)
end on

type dw_observacion from datawindow within tabpage_2
integer x = 9
integer y = 108
integer width = 2688
integer height = 848
integer taborder = 40
string title = "none"
string dataobject = "dw_mant_prodpredio_obs"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from uo_dw within w_maed_predios_cuarteles
boolean visible = false
integer x = 2830
integer y = 92
integer width = 197
integer height = 144
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_cuartelvariedad"
boolean vscrollbar = false
boolean border = false
end type

