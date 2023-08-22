$PBExportHeader$w_maed_ctlcalplanillareclamos.srw
$PBExportComments$Ingresador de antecedentes para Planilla Reclamos por Especie.
forward
global type w_maed_ctlcalplanillareclamos from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_ctlcalplanillareclamos
end type
type tabpage_1 from userobject within tab_1
end type
type pb_elimina_det from picturebutton within tabpage_1
end type
type pb_lotes from picturebutton within tabpage_1
end type
type dw_lotes from uo_dw within tabpage_1
end type
type tabpage_1 from userobject within tab_1
pb_elimina_det pb_elimina_det
pb_lotes pb_lotes
dw_lotes dw_lotes
end type
type tabpage_2 from userobject within tab_1
end type
type pb_elimina from picturebutton within tabpage_2
end type
type pb_carga from picturebutton within tabpage_2
end type
type dw_imagenes from uo_dw within tabpage_2
end type
type tabpage_2 from userobject within tab_1
pb_elimina pb_elimina
pb_carga pb_carga
dw_imagenes dw_imagenes
end type
type tabpage_3 from userobject within tab_1
end type
type pb_eliminapdf from picturebutton within tabpage_3
end type
type pb_cargapdf from picturebutton within tabpage_3
end type
type dw_pdf from uo_dw within tabpage_3
end type
type tabpage_3 from userobject within tab_1
pb_eliminapdf pb_eliminapdf
pb_cargapdf pb_cargapdf
dw_pdf dw_pdf
end type
type tab_1 from tab within w_maed_ctlcalplanillareclamos
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
end type
end forward

global type w_maed_ctlcalplanillareclamos from w_mant_encab_deta_csd
integer width = 5038
integer height = 4976
string menuname = ""
windowstate windowstate = maximized!
event ue_validapassword ( )
tab_1 tab_1
end type
global w_maed_ctlcalplanillareclamos w_maed_ctlcalplanillareclamos

type variables
CONSTANT	Integer	MaximoImagenes = 15, MaxPDF = 4
DataWindowChild	idwc_especies, idwc_recibidor, idwc_tipo, idwc_nave, idwc_puertos, idwc_variedades, idwc_embarques

Datawindow    		dw_3, dw_4, dw_5

uo_zonas          iuo_zonas
uo_variedades     iuo_variedades
uo_productores    iuo_productor
uo_especie        iuo_especies
uo_recibidores		iuo_recibidores
uo_puertos			iuo_puertos
uo_naves				iuo_naves
uo_tipotransporte	iuo_tipo
uo_embalajes		iuo_embalajes
uo_embarques		iuo_embarque
uo_Reclamos			iuo_Reclamos
uo_admdoctos		iuo_admdoctos

Integer 				ii_sw
String				is_Ruta



end variables

forward prototypes
protected function integer wf_modifica ()
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string ls_columna)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine wf_child ()
public function boolean duplicado (string valor)
public function boolean wf_grabaimagenes ()
public function boolean wf_carga_imagenes (datawindow adw)
public function integer wf_semana (date ad_fecha, transaction at_transaccion)
public function boolean wf_planilla (long planilla)
public function boolean wf_grabapdf ()
public function integer calcula_usd ()
end prototypes

event ue_validapassword();istr_mant.Argumento[1]	=	"Control de Calidad"
istr_mant.Argumento[2]	=	gstr_parlote.paswor

OpenWithParm(w_password, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF istr_mant.Respuesta = 0 THEN Close(This)
end event

protected function integer wf_modifica ();RETURN 1
end function

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.reen_numero.Protect					=	0
	dw_2.Object.reen_feczar.Protect					=	0
	dw_2.Object.reci_codigo.Protect					=	0
	dw_2.Object.nave_tipotr.Protect					=	0
	dw_2.Object.nave_codigo.Protect					=	0
	dw_2.Object.puer_codigo.Protect					=	0
	dw_2.Object.reen_semliq.Protect					=	0	
	dw_2.Object.reen_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.reen_feczar.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.reci_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.nave_tipotr.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.nave_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.puer_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.reen_semliq.BackGround.Color		=	RGB(255,255,255)
  	pb_buscar.Enabled   									= True
Else
	dw_2.Object.reen_numero.Protect					=	1
	dw_2.Object.reen_feczar.Protect					=	1
	dw_2.Object.reci_codigo.Protect					=	1
	dw_2.Object.nave_tipotr.Protect					=	1
	dw_2.Object.nave_codigo.Protect					=	1
	dw_2.Object.puer_codigo.Protect					=	1
	dw_2.Object.reen_semliq.Protect					=	1	
	dw_2.Object.reen_numero.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.reen_feczar.BackGround.Color		=	RGB(192,192,192)
	dw_2.Object.reci_codigo.BackGround.Color		=	RGB(192,192,192)
	dw_2.Object.nave_tipotr.BackGround.Color		=	RGB(192,192,192)
	dw_2.Object.nave_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.puer_codigo.BackGround.Color		=	RGB(192,192,192)
	dw_2.Object.reen_semliq.BackGround.Color		=	RGB(192,192,192)
	pb_buscar.Enabled   									= False
End If
end subroutine

public subroutine habilitaingreso (string ls_columna);Boolean	lb_Estado = True

dw_2.AcceptText()

If ls_Columna <> "reen_fecrec" And IsNull(dw_2.Object.reen_fecrec[1])  Then
	lb_Estado	=	False
End If

If ls_Columna <> "reci_codigo" And &
	(dw_2.Object.reci_codigo[1]) = 0 Or IsNull(dw_2.Object.reci_codigo[1])  Then
	lb_Estado	=	False
End If

If ls_Columna <> "nave_tipotr" And &
	(dw_2.Object.nave_tipotr[1]) = '' Or IsNull(dw_2.Object.nave_tipotr[1])  Then
	lb_Estado	=	False
End If

If ls_Columna <> "nave_codigo" And &
	(dw_2.Object.nave_codigo[1]) = 0 Or IsNull(dw_2.Object.nave_codigo[1])  Then
	lb_Estado	=	False
End If

If ls_Columna <> "puer_codigo" And &
	(dw_2.Object.puer_codigo[1]) = 0 Or IsNull(dw_2.Object.puer_codigo[1])  Then
	lb_Estado	=	False
End If

pb_grabar.Enabled							=	lb_Estado
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	If dw_5.Update(True, False) = 1 THEN
		If dw_4.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				IF dw_2.Update(True, False) = 1 THEN
					Commit;
				
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
					ELSE
						lb_Retorno	=	True
					
						dw_5.ResetUpdate()
						dw_4.ResetUpdate()
						dw_3.ResetUpdate()
						dw_2.ResetUpdate()
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
					
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
		
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
				IF dw_5.Update(True, False) = 1 THEN
					Commit;				
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
					ELSE
						lb_Retorno	=	True
		
						dw_2.ResetUpdate()
						dw_3.ResetUpdate()
						dw_4.ResetUpdate()
						dw_5.ResetUpdate()
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				END IF
			ELSE
	  		F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
 		RollBack;
	END IF
		ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
 		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

public subroutine wf_child ();dw_2.GetChild("espe_codigo", idwc_especies)
idwc_especies.SetTransObject(Sqlca)
If idwc_especies.Retrieve() = 0 Then idwc_especies.InsertRow(0)

dw_2.GetChild("reci_codigo", idwc_recibidor)
idwc_recibidor.SetTransObject(Sqlca)
If idwc_recibidor.Retrieve() = 0 Then idwc_recibidor.InsertRow(0)
idwc_recibidor.SetSort('reci_nombre')
idwc_recibidor.Sort()

dw_2.GetChild("embq_codigo", idwc_embarques)
idwc_embarques.SetTransObject(Sqlca)
If idwc_embarques.Retrieve(0) = 0 Then idwc_embarques.InsertRow(0)
idwc_embarques.SetSort('embq_codigo')
idwc_embarques.Sort()


dw_2.GetChild("nave_tipotr", idwc_tipo)
idwc_tipo.SetTransObject(Sqlca)
If idwc_tipo.Retrieve() = 0 Then idwc_tipo.InsertRow(0)

dw_2.GetChild("nave_codigo", idwc_nave)
idwc_nave.SetTransObject(Sqlca)
If idwc_nave.Retrieve(iuo_tipo.Codigo,-1) = 0 Then idwc_nave.InsertRow(0)
idwc_nave.SetSort('nave_nombre')
idwc_nave.Sort()

dw_2.GetChild("puer_codigo", idwc_puertos)
idwc_puertos.SetTransObject(Sqlca)
If idwc_puertos.Retrieve(-1) = 0 Then idwc_puertos.InsertRow(0)
idwc_puertos.SetSort('puer_nombre')
idwc_puertos.Sort()

dw_3.getChild('vari_codigo', idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
If idwc_variedades.Retrieve(iuo_Especies.Codigo) = 0 Then idwc_variedades.InsertRow(0)
end subroutine

public function boolean duplicado (string valor);Long		ll_fila
Date		ld_fecha

ld_fecha = Date(valor)

//ll_fila	= dw_8.Find ("String(pmde_fecham) = '" + String(ld_fecha) + "'", 1, dw_8.RowCount())

IF ll_fila > 0  THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public function boolean wf_grabaimagenes ();Boolean	lb_Retorno = True
Long		ll_Fila
String		ls_Archivo

If dw_4.RowCount() < 1 Then
	lb_Retorno = False
Else
	For ll_Fila = 1 To dw_4.RowCount()
		ls_Archivo	=	dw_4.Object.reid_ruta[ll_Fila] + dw_4.Object.reid_archiv[ll_Fila]
		
		iuo_Reclamos.GrabaImagen(ls_Archivo, dw_4.Object.reen_numero[ll_Fila], dw_4.Object.espe_codigo[ll_Fila], &
												dw_4.Object.clie_codigo[ll_Fila], dw_4.Object.reid_codigo[ll_Fila], Sqlca)
	Next
End If

Return lb_Retorno
end function

public function boolean wf_carga_imagenes (datawindow adw);Boolean	lb_Retorno = True
Long		ll_Fila, ll_New

adw.Insertrow(0)
adw.Object.nombre[1] = ''

For ll_Fila = 1 To MaximoImagenes
	If ll_Fila <= dw_4.RowCount() Then
		If FileExists(dw_4.Object.reid_ruta[ll_Fila] + dw_4.Object.reid_archiv[ll_Fila]) Then
			adw.Modify('p_' + String(ll_Fila) + ".FileName = '" + dw_4.Object.reid_ruta[ll_Fila] + dw_4.Object.reid_archiv[ll_Fila] + "'")
		Else
			adw.Modify('p_' + String(ll_Fila) + ".FileName = '" + iuo_Reclamos.generadocumento(dw_4, ll_Fila, Sqlca) + "'")
		End if
	Else
		adw.Modify('p_' + String(ll_Fila) + '.FileName = ""')
	End If
Next

Return lb_Retorno
end function

public function integer wf_semana (date ad_fecha, transaction at_transaccion);Integer	li_Retorno

Declare Calculo_Semana Procedure For dbo.semana @Fecha = :ad_Fecha USING sqlca;

Execute Calculo_Semana;
Fetch Calculo_Semana Into :li_Retorno;

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Error en calculo de semana.")
	li_Retorno = -1
End If

Close Calculo_semana;

Return li_Retorno
end function

public function boolean wf_planilla (long planilla);Long    ll_existe
Integer li_especie

ii_sw = 0

Select		reen_numero, espe_codigo
	Into		:ll_existe, :li_especie
	From		dbo.ctlcalreclamosenca
	Where	clie_codigo = :gi_codexport
	And 		reen_numero = :Planilla
	And		espe_codigo = :iuo_Especies.Codigo
	Using 		Sqlca;

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura tabla ctlcalreclamosenca  ")
	Return False
ElseIf sqlca.sqlcode	=	0 Then

	If li_especie = dw_2.Object.espe_codigo[1] Then
		istr_busq.argum[1] = String(Planilla)
		ii_sw = 0
	Else
		ii_sw = 1
	End If
	
	Return True
Else
	Return False
End If
end function

public function boolean wf_grabapdf ();Boolean	lb_Retorno = True
Long		ll_Fila
String		ls_Archivo

If dw_5.RowCount() < 1 Then
	lb_Retorno = False
Else
	For ll_Fila = 1 To dw_5.RowCount()
		ls_Archivo	=	dw_5.Object.repd_ruta[ll_Fila] + dw_5.Object.repd_archiv[ll_Fila]
		
		iuo_Reclamos.GrabaPdf(ls_Archivo, dw_5.Object.reen_numero[ll_Fila], dw_5.Object.espe_codigo[ll_Fila], &
												dw_5.Object.clie_codigo[ll_Fila], dw_5.Object.repd_codigo[ll_Fila], Sqlca)
	Next
End If

Return lb_Retorno
end function

public function integer calcula_usd ();long ll_cantlote
ll_cantlote = long(tab_1.Tabpage_1.dw_lotes.Object.compute_2[1])

If(Isnull(ll_cantlote) or ll_cantlote = 0) Then
	Return 1
Else
	If Not(IsNull(dw_2.Object.reen_vtacaj[1]) or dw_2.Object.reen_vtacaj[1] = 0) Then
		dw_2.Object.reen_vtatot[1] = long(dw_2.Object.reen_vtacaj[1]) * ll_cantlote
	End If

	If Not(IsNull(dw_2.Object.reen_retcaj[1]) or dw_2.Object.reen_retcaj[1] = 0) Then
		dw_2.Object.reen_rettot[1] = long(dw_2.Object.reen_retcaj[1]) * ll_cantlote
	End If
End If

Return 0
end function

on w_maed_ctlcalplanillareclamos.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_maed_ctlcalplanillareclamos.destroy
call super::destroy
destroy(this.tab_1)
end on

event ue_seleccion;call super::ue_seleccion;istr_busq.argum[1]	=	String(iuo_Especies.Codigo)
istr_busq.argum[2]	=	String(gi_CodExport)

OpenWithParm(w_busc_ctlcalplanillareclamos, istr_busq)

istr_busq = Message.PowerObjectParm

If UpperBound(istr_busq.argum) > 2 Then
	If istr_busq.argum[3] <> "" Then
		iuo_Especies.Codigo		= Integer(istr_Busq.Argum[1])
		istr_mant.Argumento[1]	= istr_Busq.Argum[3]
		
		This.TriggerEvent("ue_recuperadatos")
		Habilitaingreso('espe_codigo')
	Else
		pb_buscar.SetFocus()
		HabilitaEncab(True)
	End If
End If
		
		
end event

event ue_recuperadatos;Long 		ll_fila_e, Respuesta

dw_2.SetTransObject(Sqlca)


Do	
	ll_fila_e	= dw_2.Retrieve(gi_codexport, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))

	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	Else
		If ll_fila_e > 0 Then
			Do
				dw_5.Retrieve(gi_codexport, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
				dw_4.Retrieve(gi_codexport, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
				dw_3.Retrieve(gi_codexport, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
				
				iuo_Especies.Codigo	=	dw_2.Object.espe_codigo[1]
				iuo_Tipo.Codigo		=	dw_2.Object.nave_tipotr[1]
				
				wf_Child()
				
			Loop While Respuesta = 1
			calcula_usd()
			pb_imprimir.Enabled	= True
			pb_grabar.Enabled	= True
			pb_eliminar.Enabled	= True
		End If
		
		If Respuesta = 2 Then Close(This)
	End If
Loop While Respuesta = 1

If Respuesta = 2 Then Close(This)
end event

event ue_nuevo;Integer  	li_Grupo, li_null
Long		ll_modif2, ll_modif3, ll_modif4, ll_modif5

SetNull(li_null)
ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif2		=	dw_2.GetNextModified(0, Primary!)
			ll_modif3		=	dw_3.GetNextModified(0, Primary!)
			ll_modif4		=	dw_4.GetNextModified(0, Primary!)
			ll_modif5		=	dw_5.GetNextModified(0, Primary!)
						
			IF dw_3.RowCount() > 0 Or dw_4.RowCount() > 0 Or dw_5.RowCount() > 0  THEN
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

dw_3.Reset()
dw_4.Reset()
dw_5.Reset()

pb_grabar.Enabled		=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

HabilitaEncab(True)

dw_2.Object.espe_codigo[1]	=	iuo_Especies.Codigo
dw_2.Object.reen_fecrec[1]		=	Today()
li_Grupo = BuscaGrupo(Upper(Gstr_Us.Nombre))

If (li_Grupo > 2) Then
	pb_eliminar.Visible	=	False
	TriggerEvent('resize')
End If

dw_2.SetFocus()
dw_2.SetColumn("reen_numero")
end event

event open;x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

pb_nuevo.PostEvent(Clicked!)

dw_3		=	tab_1.tabpage_1.dw_lotes
dw_4		=	tab_1.tabpage_2.dw_imagenes
dw_5		=	tab_1.tabpage_3.dw_pdf

//istr_mant.dw	=	dw_4

dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)

iuo_variedades  	= Create	uo_variedades 
iuo_zonas       		= Create	uo_zonas
iuo_productor   	= Create	uo_productores
iuo_embalajes		= Create uo_embalajes
iuo_especies   		= Create	uo_especie
iuo_recibidores		= Create	uo_recibidores
iuo_puertos			= Create uo_puertos
iuo_naves			= Create uo_naves
iuo_tipo				= Create uo_tipotransporte
iuo_embarque		= Create	uo_embarques
iuo_Reclamos		= Create uo_Reclamos
iuo_admdoctos		= Create uo_admdoctos

If Not iuo_Especies.Existe(Integer(Message.StringParm), True, Sqlca) Then Return
This.Title	= "Planilla Reclamos: " + iuo_especies.Nombre
iuo_Tipo.Codigo = '*'
iuo_recibidores.Codigo	= -1

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, is_Ruta)

wf_child()

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long	fila, ll_Fila, ll_New
string ls_archivo

istr_info.titulo	=	"INFORME DE RECLAMOS"
istr_info.copias	=	1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject	=	"dw_info_ctlcalplanillareclamos"
vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(gi_codexport, iuo_Especies.Codigo, dw_2.Object.reen_numero[1], dw_2.Object.reen_fecrec[1], dw_2.Object.reen_fecrec[1], -1, -1, -1, -1)

If fila	=	-1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila	=	0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Zoom = 95')
	
	If dw_4.RowCount() = 0 Then
		vinf.dw_1.Object.Compute_3.Expression = "'Sin Fotos.'"
	Else
		If MessageBox('Atención', 'Desea imprimir Fotos.', Exclamation!, YesNo!, 2) = 1 Then
			If wf_carga_imagenes(dw_1) Then
				dw_1.Object.t_52.text = dw_1.Object.t_52.text + ' - Planilla - ' + String(dw_2.Object.reen_numero[1])
				dw_1.Print()
			End If
		End If
	End If

	If dw_5.RowCount() > 0 Then
		If MessageBox('Atención', 'Desea imprimir documentos anexos..', Exclamation!, YesNo!, 2) = 1 Then
			For ll_fila = 1 to dw_5.RowCount()
					ls_archivo=string(dw_5.Object.repd_ruta[ll_fila]+ dw_5.Object.repd_archiv[ll_fila])
					If FileExists(ls_Archivo) Then 
						iuo_Reclamos.AbrirDocumento(ls_Archivo)
					Else
						iuo_Reclamos.RecuperaPDF(dw_5, Sqlca)
					End If
			Next
		End If
	End If

	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
End If

SetPointer(Arrow!)
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	Tab_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	Tab_1.width
END IF

dw_2.x					= 37 + Round((This.WorkSpaceWidth() - dw_2.width - 400) / 2, 0)
dw_2.y					= 37

Tab_1.width				= This.WorkSpaceWidth() - 400

Tab_1.x					= 37 + Round((This.WorkSpaceWidth() - 400 - Tab_1.width ) / 2, 0)
Tab_1.y					= 64 + dw_2.Height

Tab_1.Height			= This.WorkSpaceHeight() - Tab_1.y - 41

Tab_1.TabPage_1.dw_lotes.Height	=	Tab_1.Height -	250
Tab_1.TabPage_1.dw_lotes.Width	=	Tab_1.Width -	360
Tab_1.TabPage_1.pb_lotes.x			=	Tab_1.TabPage_1.dw_lotes.x + Tab_1.TabPage_1.dw_lotes.Width + 20
Tab_1.TabPage_1.pb_elimina_det.x	=	Tab_1.TabPage_1.dw_lotes.x + Tab_1.TabPage_1.dw_lotes.Width + 20

Tab_1.TabPage_2.dw_Imagenes.Height	=	Tab_1.Height -	250
Tab_1.TabPage_2.dw_Imagenes.Width	=	Tab_1.Width -	360
Tab_1.TabPage_2.pb_Carga.x				=	Tab_1.TabPage_2.dw_Imagenes.x + Tab_1.TabPage_2.dw_Imagenes.Width + 20
Tab_1.TabPage_2.pb_elimina.x			=	Tab_1.TabPage_2.dw_Imagenes.x + Tab_1.TabPage_2.dw_Imagenes.Width + 20

Tab_1.TabPage_3.dw_pdf.Height	=	Tab_1.Height -	250
Tab_1.TabPage_3.dw_pdf.Width	=	Tab_1.Width -	360
Tab_1.TabPage_3.pb_Cargapdf.x		=	Tab_1.TabPage_3.dw_pdf.x + Tab_1.TabPage_3.dw_pdf.Width + 20
Tab_1.TabPage_3.pb_eliminapdf.x		=	Tab_1.TabPage_3.dw_pdf.x + Tab_1.TabPage_3.dw_pdf.Width + 20

//gb_1.x 					= This.WorkSpaceWidth() - 310
//gb_1.y 					= 5
//gb_1.width				= 275

li_posic_x				= This.WorkSpaceWidth() - 250
//li_posic_y				= gb_1.y + 88

IF pb_buscar.Visible THEN
	pb_buscar.x				= li_posic_x - 50
	pb_buscar.y				= li_posic_y + 20
//	pb_buscar.width		= 235
//	pb_buscar.height		= 195
	li_visible ++
	li_posic_y += 250
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x - 50
	pb_nuevo.y				= li_posic_y + 20
//	pb_nuevo.width		= 235
//	pb_nuevo.height		= 195
	li_visible ++
	li_posic_y += 250
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			= li_posic_x  - 50
	pb_eliminar.y			= li_posic_y + 30
//	pb_eliminar.width		= 235
//	pb_eliminar.height		= 195
	li_visible ++
	li_posic_y += 250
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				= li_posic_x - 50
	pb_grabar.y				= li_posic_y + 40
//	pb_grabar.width		= 235
//	pb_grabar.height		= 195
	li_visible ++
	li_posic_y += 250
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x - 50
	pb_imprimir.y			= li_posic_y + 50
//	pb_imprimir.width		= 235
//	pb_imprimir.height	= 195
	li_visible ++
	li_posic_y += 250
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x - 50
	pb_salir.y				= li_posic_y + 60
//	pb_salir.width			= 235
//	pb_salir.height			= 195
	li_visible ++
	li_posic_y += 250
END IF

//gb_1.height				= 180 * li_visible + 97 /*  (Según Botones Visibles)  */
//gb_2.x 					= gb_1.x	
//gb_2.y 					= 1293
//gb_2.width				= 275
//gb_2.height				= 180 * 2 + 97 /*  (2 Botones)  */

pb_ins_det.x			= li_posic_x
//pb_ins_det.y			= gb_2.y + 93
pb_ins_det.width		= 235
pb_ins_det.height		= 195

pb_eli_det.x				= li_posic_x
pb_eli_det.y				= pb_ins_det.y + 180
pb_eli_det.width		= 235
pb_eli_det.height		= 195
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount()  > 0	THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
IF dw_4.RowCount()  > 0	THEN dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF

end event

event ue_antesguardar;call super::ue_antesguardar;	Long		ll_Fila, ll_Numero, ll_Numero2
Integer	li_cont
String		ls_colu[], ls_mensaje

If dw_2.RowCount() > 0 Then
	If IsNull(dw_2.Object.reen_numero[1]) Then  dw_2.Object.reen_numero[1] = iuo_Reclamos.Maximo(iuo_Especies.Codigo, gi_CodExport, Sqlca) + 1
	dw_2.Object.clie_codigo[1] = gi_CodExport

	If Isnull(dw_2.Object.reen_semliq[1]) OR dw_2.Object.reen_semliq[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nSemana de Liquidacion."
		ls_colu[li_cont]	= "reen_semliq"
	End If
	
	If Isnull(dw_2.Object.reen_feczar[1]) Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nFecha de Zarpe."
		ls_colu[li_cont]	= "reen_feczar"
	End If
End If

If li_cont > 0 Then
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_2.SetColumn(ls_colu[1])
		dw_2.SetFocus()
		HabilitaEncab(True)
		Message.DoubleParm = -1
		Return
Else
	ll_Numero = iuo_Reclamos.Maximo(dw_2.Object.reen_numero[1], iuo_Especies.Codigo, gi_CodExport, Sqlca) + 1
	ll_Numero2 = iuo_Reclamos.MaxPDF(dw_2.Object.reen_numero[1], iuo_Especies.Codigo, gi_CodExport, Sqlca) + 1
	For ll_Fila = 1 to dw_3.RowCount()
		If dw_3.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THen 
			dw_3.Object.clie_codigo[ll_Fila]		=	gi_CodExport
			dw_3.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
			dw_3.Object.reen_numero[ll_Fila]		=	dw_2.Object.reen_numero[1]
		End If
	Next	
	
	For ll_Fila = 1 to dw_4.RowCount()
		If dw_4.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THen 
			dw_4.Object.clie_codigo[ll_Fila]		=	gi_CodExport
			dw_4.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
			dw_4.Object.reen_numero[ll_Fila]		=	dw_2.Object.reen_numero[1]
			dw_4.Object.reid_codigo[ll_Fila]		=	ll_Numero
			ll_Numero++
		End If 
	Next
		
	For ll_Fila = 1 to dw_5.RowCount()
		If dw_5.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THen 
			dw_5.Object.clie_codigo[ll_Fila]		=	gi_CodExport
			dw_5.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
			dw_5.Object.reen_numero[ll_Fila]		=	dw_2.Object.reen_numero[1]
			dw_5.Object.repd_codigo[ll_Fila]		=	ll_Numero2
			ll_Numero2++
		End If 
	Next
End If
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	wf_GrabaImagenes()
	wf_GrabaPdf()
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_ctlcalplanillareclamos
boolean visible = false
integer x = 3717
integer y = 1696
integer width = 165
integer height = 148
boolean enabled = false
boolean titlebar = false
string title = ""
string dataobject = "dw_info_ctlcalreclamosimagenes"
boolean hscrollbar = false
boolean vscrollbar = false
boolean livescroll = false
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_ctlcalplanillareclamos
integer x = 73
integer y = 56
integer width = 3858
integer height = 852
integer taborder = 10
string dataobject = "dw_mant_ctlcalreclamosenca"
boolean controlmenu = true
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula

SetNull(ls_Nula)
ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "reen_numero"
		If wf_Planilla(Long(data)) Then
			istr_mant.Argumento[1]	=	Data
			If ii_sw = 0 Then
				Parent.TriggerEvent("ue_recuperadatos")
			Else
				Messagebox("Atención","Planilla Digitada existe para otra Especie, no puede ingresar", Exclamation!, Ok!)
				dw_2.Object.reen_numero.Protect	=	0
				dw_2.SetItem(Row,ls_Columna,Long(ls_Nula))
				Return 1
			End If
			dw_2.SetColumn(ls_Columna)
			dw_2.SetFocus()
		Else
			dw_2.Object.reen_numero.Protect	=	0
			dw_2.SetItem(Row,ls_Columna,Long(ls_Nula))
			Return 1
		End If
		
	Case "espe_codigo"
		If Not iuo_Especies.Existe(Integer(Data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		End If
		
	Case "embq_codigo"
		If Not iuo_Embarque.Existe(Data, True, Sqlca) Then
			This.Object.reci_codigo[Row]	= Long(ls_Nula)
			This.Object.puer_codigo[Row]	= Long(ls_Nula)
			This.Object.nave_tipotr[Row]	= ls_Nula
			This.Object.nave_codigo[Row]	= Long(ls_Nula)
			This.Object.reen_feczar[Row]	= Date(ls_Nula)
			This.Object.reen_semliq[Row]	= Long(ls_Nula)
		Else
			This.Object.reci_codigo[Row]	= iuo_Embarque.Recibidor
			This.Object.puer_codigo[Row]	= iuo_Embarque.PuertoDestino
			This.Object.nave_tipotr[Row]	= iuo_Embarque.TipoTransporte
			This.Object.nave_codigo[Row]	= iuo_Embarque.CodigoNave
			This.Object.reen_feczar[Row]	= iuo_Embarque.FechaZarpe
			This.Object.reen_semliq[Row]	= wf_Semana(iuo_Embarque.FechaZarpe, Sqlca)
			This.GetChild('nave_codigo', idwc_nave)
			idwc_nave.SetTransObject(Sqlca)
			If idwc_nave.Retrieve( iuo_Embarque.TipoTransporte, iuo_Embarque.Recibidor) = 0 Then idwc_nave.InsertRow(0)
		End if
		
	Case 'reci_codigo'
		If Not iuo_recibidores.Existe(Long(Data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			This.GetChild('nave_codigo', idwc_nave)
			idwc_nave.SetTransObject(Sqlca)
			If idwc_nave.Retrieve( iuo_Tipo.Codigo,Long(Data)) = 0 Then idwc_nave.InsertRow(0)
		End If
		
	Case 'puer_codigo'
		If Not iuo_Puertos.Existe(Long(Data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		End If
		
	Case 'nave_tipotr'
		If Not iuo_Tipo.existe(Data, True, sqlca) Then
			This.SetItem(row, ls_Columna, ls_nula)
			Return 1
		Else
			This.Object.nave_codigo[Row]	=	Long(ls_Nula)
			This.Object.reen_feczar[Row]	= 	Date(ls_Nula)
			This.GetChild('nave_codigo', idwc_nave)
			idwc_nave.SetTransObject(Sqlca)
			If idwc_nave.Retrieve(data,  iuo_recibidores.Codigo) = 0 Then idwc_nave.InsertRow(0)
		End If
		
	Case 'nave_codigo'
		if 	IsNull(This.Object.nave_tipotr[Row]) Then 
			MessageBox("Atención", "Debe Seleccionar Tipo Transporte Previamente",Exclamation!)
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		ElseIf Not iuo_naves.Existe(Long(Data), iuo_Tipo.Codigo, True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			If iuo_Embarque.Existe(iuo_Tipo.Codigo, Long(Data), iuo_Recibidores.Codigo, True, Sqlca) Then
				This.Object.embq_codigo[Row] =	iuo_Embarque.Codigo
				This.Object.reci_codigo[Row] 	=  iuo_Embarque.Recibidor
				This.Object.reen_feczar[Row]	= iuo_Embarque.FechaZarpe
				This.Object.puer_codigo[Row]	= iuo_Embarque.PuertoDestino
				This.Object.reen_semliq[Row]	= wf_Semana(iuo_Embarque.FechaZarpe, Sqlca)
			End if
		End If
		
//	Case 'reen_fecrec'
//		If Date(Data) <  This.Object.reen_feczar[Row] Then
//			MessageBox('Atencion', 'Fecha reclamo no puede ser anterior a Fecha zarpe', Information!, Ok!)
//		End If
//
//	Case 'reen_mailre'
//		If Date(Data) <  This.Object.reen_fecrec[Row] Then
//			MessageBox('Atencion', 'Fecha Mail Referencia no puede ser anterior a Fecha Reclamo', Information!, Ok!)
//		End If
//
//	Case 'reen_feczon'
//		If Date(Data) < Date(This.Object.reen_mailre[Row]) Then
//			MessageBox('Atencion', 'Fecha Informe a Zonal no puede ser anterior a Fecha mail referencia.', Information!, Ok!)
//		End If

	Case 'reen_fecrec'
		If Date(Data) >  date(This.Object.reen_mailre[Row]) Then
			This.Object.reen_fecrec.color = 255
		Else
			This.Object.reen_fecrec.color = 0
		End If
		If Date(Data) > date(Today()) Then
			MessageBox("Atención","La fecha de reclamo no puede ser mayor a la fecha actual")
			This.SetItem(Row,ls_Columna,Date(Today()))
			This.SetColumn(ls_Columna)
			This.SetFocus()
			Return 1
		End If
	Case 'reen_feczar'
		This.Object.reen_semliq[Row]	= wf_Semana(Date(data), Sqlca)
		
	Case 'reen_infzon'
		If Data = '1' Then 
			This.Object.reen_feczon[Row]  = Today()
		Else
			This.Object.reen_feczon[Row] = Date(ls_Nula)
		End If
				
	Case 'reen_vtacaj' ,  'reen_retcaj'
		This.AcceptText()
		calcula_usd()
		
End Choose

Habilitaingreso(ls_Columna) 
end event

event dw_2::sqlpreview;//
end event

event dw_2::buttonclicked;call super::buttonclicked;String	ls_Boton, ls_Nula
str_Busqueda lstr_busq

SetNull(ls_Nula)
ls_Boton	=	dwo.Name

lstr_busq.Argum[1]	=	String(This.Object.reci_codigo[Row])
lstr_busq.Argum[2]	=	This.Object.nave_tipotr[Row]
lstr_busq.Argum[3]	=	String(This.Object.nave_codigo[Row])
lstr_busq.Argum[4]	=	String(This.Object.puer_codigo[Row])

Choose Case ls_Boton
	Case "b_zarpe"
		OpenWithParm(w_busc_ctlcalfechaszarpe, lstr_busq)

		lstr_busq = Message.PowerObjectParm
		
		If UpperBound(lstr_busq.Argum) = 5 Then
			If lstr_busq.Argum[1] <> "" Then
				This.GetChild('nave_codigo', idwc_nave)
				idwc_nave.SetTransObject(Sqlca)
				If idwc_nave.Retrieve(lstr_busq.Argum[1]) = 0 Then idwc_nave.InsertRow(0)
				
				This.Object.nave_tipotr[Row]	= lstr_busq.Argum[1]
				This.Object.nave_codigo[Row]	= Long(lstr_busq.Argum[2])
				This.Object.reen_feczar[Row]	= Date(lstr_busq.Argum[3])
				This.Object.reci_codigo[Row]	= Long(lstr_busq.Argum[4])
				This.Object.puer_codigo[Row]	= Long(lstr_busq.Argum[5])
				This.Object.reen_semliq[Row]	= wf_Semana(Date(lstr_busq.Argum[6]), Sqlca)
				
			End If
		End If
		
End Choose
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_ctlcalplanillareclamos
string tag = ""
integer x = 4718
integer y = 312
integer taborder = 40
boolean bringtotop = false
integer textsize = 0
integer weight = 0
fontpitch fontpitch = default!
fontfamily fontfamily = anyfont!
string facename = ""
string powertiptext = ""
long backcolor = 0
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_ctlcalplanillareclamos
string tag = ""
integer x = 4718
integer y = 492
integer taborder = 60
boolean bringtotop = false
integer textsize = 0
integer weight = 0
fontpitch fontpitch = default!
fontfamily fontfamily = anyfont!
string facename = ""
string picturename = "\Desarrollo 12\Imagenes\Botones\EliminaEnab.png"
string powertiptext = ""
long backcolor = 0
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_ctlcalplanillareclamos
string tag = ""
integer x = 4718
integer y = 676
integer taborder = 50
boolean bringtotop = false
integer textsize = 0
integer weight = 0
fontpitch fontpitch = default!
fontfamily fontfamily = anyfont!
string facename = ""
string powertiptext = ""
long backcolor = 0
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_ctlcalplanillareclamos
string tag = ""
integer x = 4718
integer y = 848
integer taborder = 70
boolean bringtotop = false
integer textsize = 0
integer weight = 0
fontpitch fontpitch = default!
fontfamily fontfamily = anyfont!
string facename = ""
string powertiptext = ""
long backcolor = 0
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_ctlcalplanillareclamos
string tag = ""
integer x = 4718
integer y = 1032
integer taborder = 80
boolean bringtotop = false
integer textsize = 0
integer weight = 0
fontpitch fontpitch = default!
fontfamily fontfamily = anyfont!
string facename = ""
boolean cancel = false
string powertiptext = ""
long backcolor = 0
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_ctlcalplanillareclamos
string tag = ""
boolean visible = false
integer x = 4727
integer y = 1328
integer width = 0
integer height = 0
integer taborder = 0
boolean bringtotop = false
integer textsize = 0
integer weight = 0
fontpitch fontpitch = default!
fontfamily fontfamily = anyfont!
string facename = ""
string picturename = ""
string disabledname = ""
string powertiptext = ""
long backcolor = 0
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_ctlcalplanillareclamos
string tag = ""
boolean visible = false
integer x = 4722
integer y = 1496
integer width = 0
integer height = 0
integer taborder = 0
boolean bringtotop = false
integer textsize = 0
integer weight = 0
fontpitch fontpitch = default!
fontfamily fontfamily = anyfont!
string facename = ""
string picturename = ""
string disabledname = ""
string powertiptext = ""
long backcolor = 0
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_ctlcalplanillareclamos
string tag = ""
integer x = 4713
integer y = 136
integer taborder = 30
boolean bringtotop = false
integer textsize = 0
integer weight = 0
fontpitch fontpitch = default!
fontfamily fontfamily = anyfont!
string facename = ""
string powertiptext = ""
long backcolor = 0
end type

type tab_1 from tab within w_maed_ctlcalplanillareclamos
integer x = 73
integer y = 952
integer width = 4489
integer height = 1636
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 30586022
boolean multiline = true
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
alignment alignment = center!
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
end type

on tab_1.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.tabpage_3=create tabpage_3
this.Control[]={this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3}
end on

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
destroy(this.tabpage_3)
end on

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4453
integer height = 1508
long backcolor = 30586022
string text = "Lotes"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Custom042!"
long picturemaskcolor = 536870912
pb_elimina_det pb_elimina_det
pb_lotes pb_lotes
dw_lotes dw_lotes
end type

on tabpage_1.create
this.pb_elimina_det=create pb_elimina_det
this.pb_lotes=create pb_lotes
this.dw_lotes=create dw_lotes
this.Control[]={this.pb_elimina_det,&
this.pb_lotes,&
this.dw_lotes}
end on

on tabpage_1.destroy
destroy(this.pb_elimina_det)
destroy(this.pb_lotes)
destroy(this.dw_lotes)
end on

type pb_elimina_det from picturebutton within tabpage_1
integer x = 4091
integer y = 348
integer width = 233
integer height = 196
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\SuprimirEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\SuprimirDisab.png"
long backcolor = 30586022
end type

event clicked;IF dw_3.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_3.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox('Imagenes',"No se puede borrar actual registro.")
	END IF

 IF dw_3.RowCount() = 0 THEN
		Parent.pb_elimina_det.Enabled = False
	ELSE
		il_fila = dw_3.GetRow()
	END IF
END IF
end event

type pb_lotes from picturebutton within tabpage_1
integer x = 4091
integer y = 148
integer width = 233
integer height = 196
integer taborder = 40
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Sistemas\movimiento_camara.jpg"
string disabledname = "\Desarrollo 12\Imagenes\Sistemas\movimiento_camara_bn.jpg"
long backcolor = 30586022
end type

event clicked;str_Mant	lstr_mant

lstr_mant.dw	=	dw_3

lstr_Mant.Argumento[1] = String(dw_2.Object.reci_codigo[1])
lstr_Mant.Argumento[2] = String(dw_2.Object.reen_feczar[1], 'dd/mm/yyyy')
lstr_Mant.Argumento[3] = String(dw_2.Object.nave_tipotr[1])
lstr_Mant.Argumento[4] = String(dw_2.Object.nave_codigo[1])
lstr_Mant.Argumento[5] = String(iuo_Especies.Codigo)

OpenWithParm(w_consulta_embarques, lstr_mant)

lstr_mant	= Message.PowerObjectParm

If UpperBound(lstr_mant.Argumento) > 5 Then
	lstr_mant						=	Message.PowerObjectParm
	iuo_Tipo.Codigo				=	lstr_mant.Argumento[7]
	iuo_Naves.Codigo			=	Long(lstr_mant.Argumento[8])
	iuo_Recibidores.Codigo	=	Long(lstr_mant.Argumento[6])
	iuo_Puertos.Codigo		=	Long(lstr_mant.Argumento[9])
	
	wf_Child()
	
	dw_2.Object.reci_codigo[1]		=	Integer(lstr_mant.Argumento[6])
	dw_2.Object.nave_tipotr[1]		=	lstr_mant.Argumento[7]
	dw_2.Object.nave_codigo[1]	=	Integer(lstr_mant.Argumento[8])
	dw_2.Object.puer_codigo[1]	=	Integer(lstr_mant.Argumento[9])
	dw_2.Object.reen_feczar[1]		=	Date(lstr_mant.Argumento[10])
	dw_2.Object.reen_semliq[1]		=	wf_Semana(Date(lstr_mant.Argumento[10]), Sqlca)
End If
end event

type dw_lotes from uo_dw within tabpage_1
integer x = 50
integer y = 56
integer width = 3931
integer height = 1384
integer taborder = 11
string dataobject = "dw_mant_mues_ctlcalplanillareclamoslote"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
end type

event sqlpreview;//
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4453
integer height = 1508
long backcolor = 30586022
string text = "Imagenes"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Picture5!"
long picturemaskcolor = 536870912
pb_elimina pb_elimina
pb_carga pb_carga
dw_imagenes dw_imagenes
end type

on tabpage_2.create
this.pb_elimina=create pb_elimina
this.pb_carga=create pb_carga
this.dw_imagenes=create dw_imagenes
this.Control[]={this.pb_elimina,&
this.pb_carga,&
this.dw_imagenes}
end on

on tabpage_2.destroy
destroy(this.pb_elimina)
destroy(this.pb_carga)
destroy(this.dw_imagenes)
end on

type pb_elimina from picturebutton within tabpage_2
integer x = 4155
integer y = 368
integer width = 233
integer height = 196
integer taborder = 70
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\SuprimirEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\SuprimirDisab.png"
alignment htextalign = left!
long backcolor = 30586022
end type

event clicked;IF dw_4.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_4.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox('Imagenes',"No se puede borrar actual registro.")
	END IF

 IF dw_4.RowCount() = 0 THEN
		Parent.pb_elimina.Enabled = False
	ELSE
		il_fila = dw_4.GetRow()
	END IF
END IF
end event

type pb_carga from picturebutton within tabpage_2
integer x = 4155
integer y = 160
integer width = 233
integer height = 196
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\Buscae.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\GrabaDisab.png"
alignment htextalign = left!
long backcolor = 30586022
end type

event clicked;istr_mant.Argumento[1] = String(dw_2.Object.reen_numero[1])
istr_mant.Argumento[2] = String(iuo_Especies.Codigo)
istr_mant.Respuesta = 1
istr_mant.dw	=	dw_4
OpenWithParm(w_mant_deta_ctlcalreclamosimagendeta, istr_mant)

istr_mant	= Message.PowerObjectParm
end event

type dw_imagenes from uo_dw within tabpage_2
integer x = 50
integer y = 56
integer width = 1966
integer height = 1384
integer taborder = 11
string dataobject = "dw_mues_ctlcalreclamosimagen"
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;String	ls_Archivo
If Not Row = 0 then
	ls_Archivo	=	This.Object.reid_ruta[Row]+ This.Object.reid_archiv[Row]
	
	If FileExists(ls_Archivo) Then 
		iuo_Reclamos.AbrirDocumento(ls_Archivo)
	Else
		iuo_Reclamos.RecuperaImagen(This, Sqlca)
	End If
End if

end event

event sqlpreview;//
end event

type tabpage_3 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 4453
integer height = 1508
long backcolor = 30586022
string text = "Anexos"
long tabtextcolor = 33554432
long tabbackcolor = 30586022
string picturename = "Form!"
long picturemaskcolor = 536870912
pb_eliminapdf pb_eliminapdf
pb_cargapdf pb_cargapdf
dw_pdf dw_pdf
end type

on tabpage_3.create
this.pb_eliminapdf=create pb_eliminapdf
this.pb_cargapdf=create pb_cargapdf
this.dw_pdf=create dw_pdf
this.Control[]={this.pb_eliminapdf,&
this.pb_cargapdf,&
this.dw_pdf}
end on

on tabpage_3.destroy
destroy(this.pb_eliminapdf)
destroy(this.pb_cargapdf)
destroy(this.dw_pdf)
end on

type pb_eliminapdf from picturebutton within tabpage_3
integer x = 4155
integer y = 368
integer width = 233
integer height = 196
integer taborder = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\SuprimirEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\SuprimirDisab.png"
alignment htextalign = left!
long backcolor = 30586022
end type

event clicked;IF dw_5.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_5.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox('Imagenes',"No se puede borrar actual registro.")
	END IF

 IF dw_5.RowCount() = 0 THEN
		Parent.pb_eliminapdf.Enabled = False
	ELSE
		il_fila = dw_5.GetRow()
	END IF
END IF
end event

type pb_cargapdf from picturebutton within tabpage_3
integer x = 4155
integer y = 160
integer width = 233
integer height = 196
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\Buscae.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\GrabaDisab.png"
alignment htextalign = left!
long backcolor = 30586022
end type

event clicked;istr_mant.Argumento[1] = String(dw_2.Object.reen_numero[1])
istr_mant.Argumento[2] = String(iuo_Especies.Codigo)
istr_mant.Respuesta = 2
istr_mant.dw	=	dw_5
OpenWithParm(w_mant_deta_ctlcalreclamosimagendeta, istr_mant)

istr_mant	= Message.PowerObjectParm
end event

type dw_pdf from uo_dw within tabpage_3
integer x = 50
integer y = 56
integer width = 1966
integer height = 1384
integer taborder = 10
string dataobject = "dw_mues_ctlcalreclamospdf"
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;String	ls_Archivo
If Not Row = 0 then
	ls_Archivo	=	This.Object.repd_ruta[Row]+ This.Object.repd_archiv[Row]
	
	If FileExists(ls_Archivo) Then 
		iuo_Reclamos.AbrirDocumento(ls_Archivo)
	Else
		iuo_Reclamos.RecuperaPDF(This, Sqlca)
	End If
End if

end event

event sqlpreview;//
end event

