$PBExportHeader$w_cargaembarque.srw
forward
global type w_cargaembarque from w_systray
end type
type mle_texto from multilineedit within w_cargaembarque
end type
type dw_1 from uo_dw within w_cargaembarque
end type
type cb_1 from commandbutton within w_cargaembarque
end type
type dw_3 from uo_dw within w_cargaembarque
end type
type dw_4 from uo_dw within w_cargaembarque
end type
type dw_5 from uo_dw within w_cargaembarque
end type
type dw_6 from uo_dw within w_cargaembarque
end type
end forward

global type w_cargaembarque from w_systray
integer width = 3442
integer height = 1520
string title = "Carga CargoProduce"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
mle_texto mle_texto
dw_1 dw_1
cb_1 cb_1
dw_3 dw_3
dw_4 dw_4
dw_5 dw_5
dw_6 dw_6
end type
global w_cargaembarque w_cargaembarque

type variables

Private String _URL = "https://expo-carlo.rossi.cl/web-service/consulta-embarque?"
Private String _APIKey = "sApiKey=yMSvtAf77P4x"
Private String _Referencia ="&sReferenciaCliente="
Private String _Final = "&sFechaInicio=&sFechaTermino="

end variables

forward prototypes
public function boolean wf_carga ()
public function boolean wf_archivo (string mensaje)
public subroutine wf_inserta (string mensaje)
protected function boolean wf_actualiza_db ()
public function boolean wf_validareferencia (string referencia)
public function integer wf_cargaviaje (string viaje)
public function boolean wf_conectaws (string referencia, ref string response)
end prototypes

public function boolean wf_carga ();Boolean	lb_Retorno = True
Long		ll_Fila, ll_Respuesta 
String		ls_respuesta

SetPointer(HourGlass!)

wf_Inserta('Inicio de Proceso de Carga...')
wf_Inserta('Inicia recuperacion de Referencia a cargar...')

ll_Fila	= dw_1.Retrieve()

If ll_fila = -1 Then
	wf_Inserta("No es posible conectar la Base de Datos...")
	lb_Retorno = False
ElseIf ll_fila = 0 Then
	wf_Inserta('No hay Referencia para carga...')
	wf_Inserta("Carga de Referencias terminada.")
ElseIf ll_fila > 0 Then
	wf_Inserta(String(dw_1.RowCount(), '#,##0') + ': Referencias han sido Recuperados...')
	wf_Inserta('Inicia carga de Referencias...')
	
	For ll_Fila = 1 To dw_1.RowCount()
		wf_Inserta('Carga de Referencia (' + dw_1.Object.referencia[ll_Fila] + "),  Nro. " + &
											String(ll_Fila, '#,##0') + " de " + String(dw_1.RowCount(), '#,##0'))
											
		ll_Respuesta = wf_CargaViaje(dw_1.Object.referencia[ll_Fila])
		
		If ll_Respuesta = -1 Then
			wf_Inserta('Carga de Referencia (' + dw_1.Object.referencia[ll_Fila] + "), Fallo en la carga.") 
		ElseIf ll_Respuesta = 0 Then
			wf_Inserta('Carga de Referencia (' + dw_1.Object.referencia[ll_Fila] + ", Cargado correctamente.")
			
			wf_actualiza_db()
			wf_Inserta("Actualiza Referencia  en LOG de Carga.")
		End If	
		
		Yield()
	Next
	
	wf_Inserta("Carga de Referencia terminada.")	
End If

wf_Archivo(mle_Texto.Text)

SetPointer(Arrow!) 

Return lb_Retorno


end function

public function boolean wf_archivo (string mensaje);Boolean		lb_Retorno = True
Integer		li_Fila
String			ls_Archivo
DataStore	lds_Archivo

lds_Archivo = Create DataStore
lds_Archivo.DataObject = 'dw_Archivo'

If IsNull(Mensaje) Then Mensaje = ''

li_Fila = lds_Archivo.InsertRow(0)

lds_Archivo.Object.Mensaje[li_Fila] = Mensaje

ls_Archivo = 'LOG_' + String(Today(), 'ddmmyyyyThhmm')+ '.TXT'

If lds_Archivo.SaveAs(ls_Archivo, Text!, False) = -1 Then
	lb_Retorno = False
End If

Destroy lds_Archivo

Return lb_Retorno
end function

public subroutine wf_inserta (string mensaje);If IsNull(Mensaje) Then Mensaje = ''

mle_Texto.Text += String(Today(), 'dd/mm/yyyy hh:mm:ss') + ' - ' + Mensaje + '~r~n'
mle_Texto.Scroll(mle_Texto.LineCount())

end subroutine

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_3.GrupoFecha	=	ldt_FechaHora

If Not dw_3.uf_check_required(0) Then Return False
If Not dw_3.uf_validate(0) Then Return False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If dw_3.Update(True, False) = 1 Then 
	If dw_4.Update(True, False) = 1 Then 
		If dw_5.Update(True, False) = 1 Then 
			If dw_6.Update(True, False) = 1 Then 
				Commit;
				
				If sqlca.SQLCode <> 0 Then
					//F_ErrorBaseDatos(sqlca, This.Title)
					wf_Inserta("Error al grabar: " +  SQLCA.SqlErrText)
					lb_Retorno	=	False
				Else
					lb_Retorno	=	True
					dw_3.ResetUpdate()
					dw_4.ResetUpdate()
					dw_5.ResetUpdate()
					dw_6.ResetUpdate()
				End If
			Else
				RollBack;
				
				If sqlca.SQLCode <> 0 Then wf_Inserta("Error al grabar: " +  SQLCA.SqlErrText)
				lb_Retorno	=	False
			End If
		Else
			RollBack;
			
			If sqlca.SQLCode <> 0 Then wf_Inserta("Error al grabar: " +  SQLCA.SqlErrText)
			lb_Retorno	=	False
		End If
	Else
		RollBack;
		
		If sqlca.SQLCode <> 0 Then wf_Inserta("Error al grabar: " +  SQLCA.SqlErrText)
		lb_Retorno	=	False
	End If
Else
	RollBack;
	
	If SQLCA.SQLCode <> 0 Then wf_Inserta("Error al grabar: " + SQLCA.SqlErrText)
	lb_Retorno	=	False
End If

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

public function boolean wf_validareferencia (string referencia);Boolean lb_Retorno = True
String		Refe, Despacho

SELECT referencia, Despacho
	INTO	:Refe, :Despacho
	FROM	dbo.Viaje
	WHERE	referencia	=	:Referencia;

If SQLCA.SqlCode = -1 Then
	F_ErrorBaseDatos(SQLCA,"Lectura de Viajes")
	lb_Retorno =  False
ElseIf SQLCA.SQLCode = 100 Then
	lb_Retorno =  False
End If

Return lb_Retorno
end function

public function integer wf_cargaviaje (string viaje);String Response, Resultado, Atributo, Referencia, Pallet
Byte	Estado, Largo
Long	Root, Array, Count, Index, Fila, ArrayEmbarque, IndexEmbarque, CountEmbarque, FilaE, &
		ArrayPallet, IndexPallet, CountPallet, FilaP, ArrayDetalle, IndexDetalle, CountDetalle, FilaD, New

JSONParser JSON

JSON = Create JSONParser


dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()

If wf_ConectaWS(Viaje, Response) Then
	Resultado = JSON.LoadString (Response)

	Root = JSON.GetRootItem()
	Array = JSON.GetItemArray(Root, 'Documental')
	Count = JSON.GetChildCount(Array)
	
	For Fila = 1 to Count
		Index = JSON.GetChildItem(Array, Fila)
		New	=	dw_3.InsertRow(0)
		
		Estado = Integer(JSON.GetItemString(Index, "ws_estado1"))
		Referencia = JSON.GetItemString(Index, "referencia")
		
//		If Estado = 3 Then 
//			//wf_ActualizaRef(Response)
//			wf_Inserta('Carga de Referencia (' + referencia + "), Estado 3.") 
//		ElseIf Estado = 1 Then
//			wf_Inserta('Carga de Referencia (' + referencia + "), estado 1.") 
//			Return -2
		If Estado = 2 Then
			If wf_ValidaReferencia(Referencia) Then Return -1
		End If
		 
		Atributo = JSON.GetItemString(Index, "despacho")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "despacho", Atributo)
		
		Referencia = JSON.GetItemString(Index, "referencia")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "referencia", Referencia)
		
		Atributo = JSON.GetItemString(Index, "booking")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "booking", Atributo)
		
		Atributo = JSON.GetItemString(Index, "dusfecha")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "dusfecha", Atributo)
		
		Atributo = JSON.GetItemString(Index, "dusnro")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "dusnro", Atributo)
		
		Atributo = JSON.GetItemString(Index, "dusdv")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "dusdv", Atributo)
		
		Atributo = JSON.GetItemString(Index, "consignatario")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "consignatario", Atributo)
		
		Atributo = JSON.GetItemString(Index, "puertoemb")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "puertoemb", Atributo)
		
		Atributo = JSON.GetItemString(Index, "puertodes")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "puertodes", Atributo)
		
		Atributo = JSON.GetItemString(Index, "pais")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "pais", Atributo)
		
		Atributo = JSON.GetItemString(Index, "programa")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "programa", Atributo)
		
		Atributo = JSON.GetItemString(Index, "motonave")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "motonave", Atributo)
		
		Atributo = JSON.GetItemString(Index, "viaje")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "viaje", Atributo)
		
		Atributo = JSON.GetItemString(Index, "tiponave")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "tiponave", Atributo)
		
		Atributo = JSON.GetItemString(Index, "fechainicio")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "fechainicio", Atributo)
		
		Atributo = JSON.GetItemString(Index, "horainicio")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "horainicio", Atributo)
		
		Atributo = JSON.GetItemString(Index, "fechaestzarpe")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "fechaestzarpe", Atributo)
		
		Atributo = JSON.GetItemString(Index, "horaestzarpe")
		dw_3.SetItem(New, "horaestzarpe", Atributo)
		
		Atributo = JSON.GetItemString(Index, "fechazarpe")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "fechazarpe", Atributo)
		
		Atributo = JSON.GetItemString(Index, "horazarpe")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "horazarpe", Atributo)
		
		Atributo = JSON.GetItemString(Index, "fechaenvioinforme")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "fechaenvioinforme", Atributo)
		
		Atributo = JSON.GetItemString(Index, "horaenvioinforme")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "horaenvioinforme", Atributo)
		
		Atributo = JSON.GetItemString(Index, "terminal")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "terminal", Atributo)
		
		Atributo = JSON.GetItemString(Index, "sitio")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "sitio", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_tipoflete")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_tipoflete", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_despacho_extranjero")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_despacho_extranjero", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_despacho_nacional")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_despacho_nacional", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_etadestino")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_etadestino", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_errorplanilla")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_errorplanilla", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_certificado")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_certificado", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_factura")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_factura", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_origen")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_origen", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_correccionbl")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_correccionbl", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_bl")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_bl", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_bltipo")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_bltipo", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_blnumero")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_blnumero", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_fullset")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_fullset", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_vistobueno")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_vistobueno", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_observaciones")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_observaciones", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_duslegalizada")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "c_duslegalizada", Atributo)
		
		Atributo = JSON.GetItemString(Index, "aduananombre")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "aduananombre", Atributo)
		
		Atributo = JSON.GetItemString(Index, "aduanacodigo")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "aduanacodigo", Atributo)
		
		Atributo = JSON.GetItemString(Index, "valorfob")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "valorfob", Atributo)
		
		Atributo = JSON.GetItemString(Index, "valorliquidoretorno")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "valorliquidoretorno", Atributo)
		
		Atributo = JSON.GetItemString(Index, "valorflete")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "valorflete", Atributo)
		
		Atributo = JSON.GetItemString(Index, "valorseguro")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "valorseguro", Atributo)
		
		Atributo = JSON.GetItemString(Index, "ws_estado1")
		Largo = Len(Atributo)
		dw_3.SetItem(New, "ws_estado1", Atributo)
		 
		 ArrayEmbarque = JSON.GetItemArray(Index, "embarque")
		 CountEmbarque = JSON.GetChildCount(ArrayEmbarque)
		 
		 For FilaE = 1 To CountEmbarque
			 IndexEmbarque = JSON.GetChildItem(ArrayEmbarque, FilaE)
			 New	=	dw_4.InsertRow(0)
			 
			 dw_4.SetItem(New, "referencia", Referencia)
			 
			Atributo = JSON.GetItemString(IndexEmbarque, "envio")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "envio", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "patente")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "patente", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "transportista")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "transportista", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "condicion")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "condicion", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "nrocontenedor")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "nrocontenedor", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "fechaipto")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "fechaipto", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "horaipto")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "horaipto", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "fechatemb")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "fechatemb", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "horatemb")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "horatemb", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "fechacump")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "fechacump", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "horacump")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "horacump", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "fechaicon")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "fechaicon", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "horaicon")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "horaicon", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "fechatcon")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "fechatcon", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "horatcon")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "horatcon", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "estado")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "estado", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallemb")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "pallemb", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tcajas")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "tcajas", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tkilosn")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "tkilosn", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tkilosb")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "tkilosb", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tpallet")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "tpallet", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "observaciones")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "observaciones", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_latearrival")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "costo_latearrival", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_ingresomultipuerto")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "costo_ingresomultipuerto", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_aforoaduana")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "costo_aforoaduana", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_consolidado")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "costo_consolidado", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_fzextraportuarias")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "costo_fzextraportuarias", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_inspeccionsag")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "costo_inspeccionsag", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_instalacioncortina")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "costo_instalacioncortina", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_guiaingresadazeal")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "costo_guiaingresadazeal", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tcontenedor")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "tcontenedor", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "sello1")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "sello1", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "sello2")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "sello2", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "sello3")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "sello3", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "sello4")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "sello4", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tara")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "tara", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "ventvalor")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "ventvalor", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "venttipo")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "venttipo", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tempsigno")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "tempsigno", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tempvalor")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "tempvalor", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "temptipo")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "temptipo", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "atmosfera")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "atmosfera", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "atmco2")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "atmco2", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "atmo2")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "atmo2", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "etiqueta")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "etiqueta", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallet1")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "pallet1", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "termografo1")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "termografo1", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallet2")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "pallet2", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "termografo2")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "termografo2", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallet3")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "pallet3", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "termografo3")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "termografo3", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallet4")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "pallet4", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "termografo4")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "termografo4", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallet5")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "pallet5", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "termografo5")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "termografo5", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "guia")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "guia", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "packing")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "packing", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "packing_codigo")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "packing_codigo", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "especie")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "especie", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "variedad")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "variedad", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "atributo")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "atributo", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "embalaje")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "embalaje", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "embalajetipo")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "embalajetipo", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "embalajekn")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "embalajekn", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "embalajekb")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "embalajekb", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "cajas")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "cajas", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallet")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "pallet", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pltxcjs")
			Largo = Len(Atributo)
			dw_4.SetItem(New, "pltxcjs", Atributo)
		 Next
		 
		 ArrayPallet = JSON.GetItemArray(Index, "pallets")
		 CountPallet = JSON.GetChildCount(ArrayPallet)
		 
		 For FilaP = 1 To CountPallet
			 IndexPallet = JSON.GetChildItem(ArrayPallet, FilaP)
			 New	=	dw_5.InsertRow(0)
			  
			Pallet = JSON.GetItemString(IndexPallet, "p_pallet")
			dw_5.SetItem(New, "p_pallet", Pallet)
			
			Atributo = JSON.GetItemString(IndexPallet, "p_camara")
			dw_5.SetItem(New, "p_camara", Atributo)
			
			Atributo = JSON.GetItemString(IndexPallet, "p_nivel")
			dw_5.SetItem(New, "p_nivel", Atributo)
			Atributo = JSON.GetItemString(IndexPallet, "p_contenedor")
			dw_5.SetItem(New, "p_contenedor", Atributo)
			
			Atributo = JSON.GetItemString(IndexPallet, "p_termografo")
			dw_5.SetItem(New, "p_termografo", Atributo)
			
			Atributo = JSON.GetItemString(IndexPallet, "p_temperatura")
			dw_5.SetItem(New, "p_temperatura", Atributo)
			
			Atributo = JSON.GetItemString(IndexPallet, "p_observaciones")
			dw_5.SetItem(New, "p_observaciones", Atributo)
			
			ArrayDetalle = JSON.GetItemArray(IndexPallet, "p_detalle")
			CountDetalle= JSON.GetChildCount(ArrayDetalle)
			
			For FilaD = 1 To CountDetalle
				IndexDetalle	= JSON.GetChildItem(ArrayDetalle, FilaD)
				New	=	dw_6.InsertRow(0)
				
				dw_6.SetItem(New, "p_pallet", Pallet)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_especie")
				dw_6.SetItem(New, "p_especie", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_variedad")
				dw_6.SetItem(New, "p_variedad", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_atributo")
				dw_6.SetItem(New, "p_atributo", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_cajas")
				dw_6.SetItem(New, "p_cajas", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_etiqueta")
				dw_6.SetItem(New, "p_etiqueta", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_calidad")
				dw_6.SetItem(New, "p_calidad", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_calibre")
				dw_6.SetItem(New, "p_calibre", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_fecha")
				dw_6.SetItem(New, "p_fecha", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_productor")
				dw_6.SetItem(New, "p_productor", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_codembalaje")
				dw_6.SetItem(New, "p_codembalaje", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_embalaje")
				dw_6.SetItem(New, "p_embalaje", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_kn")
				dw_6.SetItem(New, "p_kn", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_kb")
				dw_6.SetItem(New, "p_kb", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_embalajejet")
				dw_6.SetItem(New, "p_embalajejet", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_guia")
				dw_6.SetItem(New, "p_guia", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_packing")
				dw_6.SetItem(New, "p_packing", Atributo)
			Next
		Next
	Next
Else
	Return -1
End if

Return 0

end function

public function boolean wf_conectaws (string referencia, ref string response);Boolean	lb_Retorno = True
String		Request, Mensaje
Boolean	Respuesta

RestClient 		lrc_Rossi
JSONPackage 	lnv_Package

lrc_Rossi		=	Create RestClient
lnv_Package	=	Create JSONPackage 

Request = _Url + _APIKey + _Referencia + Referencia + _Final

//Request = "https://expo-carlo.rossi.cl/web-service/consulta-embarque?sApiKey=yMSvtAf77P4x&sReferenciaCliente=01947M/0518&sFechaInicio=&sFechaTermino="
//Request = "https://expo-carlo.rossi.cl/web-service/consulta-embarque?sApiKey=yMSvtAf77P4x&sReferenciaCliente=00245M/0115&sFechaInicio=&sFechaTermino="

lrc_Rossi.SetRequestHeaders("Content-Type:application/json;charset=UTF-8~r~nAccept-Encoding:gzip")
lrc_Rossi.SendGetRequest(Request, Response)

If lrc_Rossi.GetResponseStatusCode() = 200 Then
	lnv_Package.LoadString(Response)
	
	Respuesta	=	lnv_Package.GetValueBoolean( "success" )
	Mensaje		=	lnv_Package.GetValueString ( "msg" )

	If Not Respuesta Then
		wf_Inserta("La referencia: " + Referencia + ", " + Mensaje)
		//MessageBox("Informacion", Mensaje, Information!, OK!)
		lb_Retorno = False
	End if
Else
	wf_Inserta(Response + '.~nProblemas en la carga de Informacion de Referencia:' + Referencia)
//	MessageBox('Atencion', Response + '. Problemas en la carga de Informacion de Instructivos', StopSign!, Ok!)
	lb_Retorno = False
End If

Return lb_Retorno

end function

on w_cargaembarque.create
int iCurrent
call super::create
this.mle_texto=create mle_texto
this.dw_1=create dw_1
this.cb_1=create cb_1
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_6=create dw_6
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.mle_texto
this.Control[iCurrent+2]=this.dw_1
this.Control[iCurrent+3]=this.cb_1
this.Control[iCurrent+4]=this.dw_3
this.Control[iCurrent+5]=this.dw_4
this.Control[iCurrent+6]=this.dw_5
this.Control[iCurrent+7]=this.dw_6
end on

on w_cargaembarque.destroy
call super::destroy
destroy(this.mle_texto)
destroy(this.dw_1)
destroy(this.cb_1)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_6)
end on

event open;call super::open;in_tray.of_delete_icon(This, True)

dw_1.SetTransObject(SQLCA)
dw_3.SetTransObject(SQLCA)
dw_4.SetTransObject(SQLCA)
dw_5.SetTransObject(SQLCA)
dw_6.SetTransObject(SQLCA)

mle_Texto.Setfocus()
mle_Texto.Text = ''

If Not wf_Carga() Then wf_Inserta('Fallo en el proceso de carga...')

//Close(This)

end event

event resize;call super::resize;
mle_texto.x = 40
mle_texto.y = 30
mle_texto.Width = pb_salir.x - mle_texto.x - 25
mle_texto.Height= pb_salir.y + mle_texto.y + 165
end event

type pb_salir from w_systray`pb_salir within w_cargaembarque
integer x = 3013
integer y = 1000
integer height = 244
string powertiptext = "Salir"
end type

type mle_texto from multilineedit within w_cargaembarque
integer x = 41
integer y = 24
integer width = 2935
integer height = 1212
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type dw_1 from uo_dw within w_cargaembarque
boolean visible = false
integer x = 3031
integer y = 88
integer width = 233
integer height = 172
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_gene_referencia_json"
boolean vscrollbar = false
end type

type cb_1 from commandbutton within w_cargaembarque
integer x = 2971
integer y = 460
integer width = 402
integer height = 112
integer taborder = 31
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "none"
end type

event clicked;String Response, Resultado, Atributo, Referencia, Pallet
Byte	Estado
Long	Root, Array, Count, Index, Fila, ArrayEmbarque, IndexEmbarque, CountEmbarque, FilaE, &
		ArrayPallet, IndexPallet, CountPallet, FilaP, ArrayDetalle, IndexDetalle, CountDetalle, FilaD, New

JSONParser JSON

JSON = Create JSONParser

If wf_ConectaWS("01947M/0518", Response) Then
	Resultado = JSON.LoadString (Response)

	Root = JSON.GetRootItem()
	Array = JSON.GetItemArray(Root, 'Documental')
	Count = JSON.GetChildCount(Array)
	
	For Fila = 1 to Count
		Index = JSON.GetChildItem(Array, Fila)
		New	=	dw_3.InsertRow(0)
		
		Estado = Integer(JSON.GetItemString(Index, "ws_estado1"))
		Referencia = JSON.GetItemString(Index, "referencia")
		
		If Estado = 3 Then 
			//wf_ActualizaRef(Response)
			Return -1
		ElseIf Estado = 1 Then
			Return -1
		ElseIf Estado = 2 Then
			If wf_ValidaReferencia(Referencia) Then Return -1
		End If
		 
		Atributo = JSON.GetItemString(Index, "despacho")
		dw_3.SetItem(New, "despacho", Atributo)
		
		Referencia = JSON.GetItemString(Index, "referencia")
		dw_3.SetItem(New, "referencia", Referencia)
		
		Atributo = JSON.GetItemString(Index, "booking")
		dw_3.SetItem(New, "booking", Atributo)
		
		Atributo = JSON.GetItemString(Index, "dusfecha")
		dw_3.SetItem(New, "dusfecha", Atributo)
		
		Atributo = JSON.GetItemString(Index, "dusnro")
		dw_3.SetItem(New, "dusnro", Atributo)
		
		Atributo = JSON.GetItemString(Index, "dusdv")
		dw_3.SetItem(New, "dusdv", Atributo)
		
		Atributo = JSON.GetItemString(Index, "consignatario")
		dw_3.SetItem(New, "consignatario", Atributo)
		
		Atributo = JSON.GetItemString(Index, "puertoemb")
		dw_3.SetItem(New, "puertoemb", Atributo)
		
		Atributo = JSON.GetItemString(Index, "puertodes")
		dw_3.SetItem(New, "puertodes", Atributo)
		
		Atributo = JSON.GetItemString(Index, "pais")
		dw_3.SetItem(New, "pais", Atributo)
		
		Atributo = JSON.GetItemString(Index, "programa")
		dw_3.SetItem(New, "programa", Atributo)
		
		Atributo = JSON.GetItemString(Index, "motonave")
		dw_3.SetItem(New, "motonave", Atributo)
		
		Atributo = JSON.GetItemString(Index, "viaje")
		dw_3.SetItem(New, "viaje", Atributo)
		
		Atributo = JSON.GetItemString(Index, "tiponave")
		dw_3.SetItem(New, "tiponave", Atributo)
		
		Atributo = JSON.GetItemString(Index, "fechainicio")
		dw_3.SetItem(New, "fechainicio", Atributo)
		
		Atributo = JSON.GetItemString(Index, "horainicio")
		dw_3.SetItem(New, "horainicio", Atributo)
		
		Atributo = JSON.GetItemString(Index, "fechaestzarpe")
		dw_3.SetItem(New, "fechaestzarpe", Atributo)
		
		Atributo = JSON.GetItemString(Index, "horaestzarpe")
		dw_3.SetItem(New, "horaestzarpe", Atributo)
		
		Atributo = JSON.GetItemString(Index, "fechazarpe")
		dw_3.SetItem(New, "fechazarpe", Atributo)
		
		Atributo = JSON.GetItemString(Index, "horazarpe")
		dw_3.SetItem(New, "horazarpe", Atributo)
		
		Atributo = JSON.GetItemString(Index, "fechaenvioinforme")
		dw_3.SetItem(New, "fechaenvioinforme", Atributo)
		
		Atributo = JSON.GetItemString(Index, "horaenvioinforme")
		dw_3.SetItem(New, "horaenvioinforme", Atributo)
		
		Atributo = JSON.GetItemString(Index, "terminal")
		dw_3.SetItem(New, "terminal", Atributo)
		
		Atributo = JSON.GetItemString(Index, "sitio")
		dw_3.SetItem(New, "sitio", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_tipoflete")
		dw_3.SetItem(New, "c_tipoflete", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_despacho_extranjero")
		dw_3.SetItem(New, "c_despacho_extranjero", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_despacho_nacional")
		dw_3.SetItem(New, "c_despacho_nacional", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_etadestino")
		dw_3.SetItem(New, "c_etadestino", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_errorplanilla")
		dw_3.SetItem(New, "c_errorplanilla", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_certificado")
		dw_3.SetItem(New, "c_certificado", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_factura")
		dw_3.SetItem(New, "c_factura", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_origen")
		dw_3.SetItem(New, "c_origen", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_correccionbl")
		dw_3.SetItem(New, "c_correccionbl", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_bl")
		dw_3.SetItem(New, "c_bl", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_bltipo")
		dw_3.SetItem(New, "c_bltipo", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_blnumero")
		dw_3.SetItem(New, "c_blnumero", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_fullset")
		dw_3.SetItem(New, "c_fullset", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_vistobueno")
		dw_3.SetItem(New, "c_vistobueno", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_observaciones")
		dw_3.SetItem(New, "c_observaciones", Atributo)
		
		Atributo = JSON.GetItemString(Index, "c_duslegalizada")
		dw_3.SetItem(New, "c_duslegalizada", Atributo)
		
		Atributo = JSON.GetItemString(Index, "aduananombre")
		dw_3.SetItem(New, "aduananombre", Atributo)
		
		Atributo = JSON.GetItemString(Index, "aduanacodigo")
		dw_3.SetItem(New, "aduanacodigo", Atributo)
		
		Atributo = JSON.GetItemString(Index, "valorfob")
		dw_3.SetItem(New, "valorfob", Atributo)
		
		Atributo = JSON.GetItemString(Index, "valorliquidoretorno")
		dw_3.SetItem(New, "valorliquidoretorno", Atributo)
		
		Atributo = JSON.GetItemString(Index, "valorflete")
		dw_3.SetItem(New, "valorflete", Atributo)
		
		Atributo = JSON.GetItemString(Index, "valorseguro")
		dw_3.SetItem(New, "valorseguro", Atributo)
		
		Atributo = JSON.GetItemString(Index, "ws_estado1")
		dw_3.SetItem(New, "ws_estado1", Atributo)
		 
		 ArrayEmbarque = JSON.GetItemArray(Index, "embarque")
		 CountEmbarque = JSON.GetChildCount(ArrayEmbarque)
		 
		 For FilaE = 1 To CountEmbarque
			 IndexEmbarque = JSON.GetChildItem(ArrayEmbarque, FilaE)
			 New	=	dw_4.InsertRow(0)
			 
			 dw_4.SetItem(New, "referencia", Referencia)
			 
			Atributo = JSON.GetItemString(IndexEmbarque, "envio")
			dw_4.SetItem(New, "envio", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "patente")
			dw_4.SetItem(New, "patente", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "transportista")
			dw_4.SetItem(New, "transportista", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "condicion")
			dw_4.SetItem(New, "condicion", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "nrocontenedor")
			dw_4.SetItem(New, "nrocontenedor", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "fechaipto")
			dw_4.SetItem(New, "fechaipto", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "horaipto")
			dw_4.SetItem(New, "horaipto", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "fechatemb")
			dw_4.SetItem(New, "fechatemb", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "horatemb")
			dw_4.SetItem(New, "horatemb", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "fechacump")
			dw_4.SetItem(New, "fechacump", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "horacump")
			dw_4.SetItem(New, "horacump", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "fechaicon")
			dw_4.SetItem(New, "fechaicon", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "horaicon")
			dw_4.SetItem(New, "horaicon", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "fechatcon")
			dw_4.SetItem(New, "fechatcon", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "horatcon")
			dw_4.SetItem(New, "horatcon", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "estado")
			dw_4.SetItem(New, "estado", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallemb")
			dw_4.SetItem(New, "pallemb", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tcajas")
			dw_4.SetItem(New, "tcajas", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tkilosn")
			dw_4.SetItem(New, "tkilosn", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tkilosb")
			dw_4.SetItem(New, "tkilosb", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tpallet")
			dw_4.SetItem(New, "tpallet", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "observaciones")
			dw_4.SetItem(New, "observaciones", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_latearrival")
			dw_4.SetItem(New, "costo_latearrival", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_ingresomultipuerto")
			dw_4.SetItem(New, "costo_ingresomultipuerto", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_aforoaduana")
			dw_4.SetItem(New, "costo_aforoaduana", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_consolidado")
			dw_4.SetItem(New, "costo_consolidado", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_fzextraportuarias")
			dw_4.SetItem(New, "costo_fzextraportuarias", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_inspeccionsag")
			dw_4.SetItem(New, "costo_inspeccionsag", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_instalacioncortina")
			dw_4.SetItem(New, "costo_instalacioncortina", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "costo_guiaingresadazeal")
			dw_4.SetItem(New, "costo_guiaingresadazeal", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tcontenedor")
			dw_4.SetItem(New, "tcontenedor", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "sello1")
			dw_4.SetItem(New, "sello1", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "sello2")
			dw_4.SetItem(New, "sello2", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "sello3")
			dw_4.SetItem(New, "sello3", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "sello4")
			dw_4.SetItem(New, "sello4", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tara")
			dw_4.SetItem(New, "tara", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "ventvalor")
			dw_4.SetItem(New, "ventvalor", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "venttipo")
			dw_4.SetItem(New, "venttipo", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tempsigno")
			dw_4.SetItem(New, "tempsigno", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "tempvalor")
			dw_4.SetItem(New, "tempvalor", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "temptipo")
			dw_4.SetItem(New, "temptipo", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "atmosfera")
			dw_4.SetItem(New, "atmosfera", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "atmco2")
			dw_4.SetItem(New, "atmco2", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "atmo2")
			dw_4.SetItem(New, "atmo2", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "etiqueta")
			dw_4.SetItem(New, "etiqueta", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallet1")
			dw_4.SetItem(New, "pallet1", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "termografo1")
			dw_4.SetItem(New, "termografo1", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallet2")
			dw_4.SetItem(New, "pallet2", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "termografo2")
			dw_4.SetItem(New, "termografo2", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallet3")
			dw_4.SetItem(New, "pallet3", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "termografo3")
			dw_4.SetItem(New, "termografo3", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallet4")
			dw_4.SetItem(New, "pallet4", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "termografo4")
			dw_4.SetItem(New, "termografo4", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallet5")
			dw_4.SetItem(New, "pallet5", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "termografo5")
			dw_4.SetItem(New, "termografo5", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "guia")
			dw_4.SetItem(New, "guia", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "packing")
			dw_4.SetItem(New, "packing", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "packing_codigo")
			dw_4.SetItem(New, "packing_codigo", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "especie")
			dw_4.SetItem(New, "especie", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "variedad")
			dw_4.SetItem(New, "variedad", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "atributo")
			dw_4.SetItem(New, "atributo", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "embalaje")
			dw_4.SetItem(New, "embalaje", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "embalajetipo")
			dw_4.SetItem(New, "embalajetipo", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "embalajekn")
			dw_4.SetItem(New, "embalajekn", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "embalajekb")
			dw_4.SetItem(New, "embalajekb", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "cajas")
			dw_4.SetItem(New, "cajas", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pallet")
			dw_4.SetItem(New, "pallet", Atributo)
			
			Atributo = JSON.GetItemString(IndexEmbarque, "pltxcjs")
			dw_4.SetItem(New, "pltxcjs", Atributo)
		 Next
		 
		 ArrayPallet = JSON.GetItemArray(Index, "pallets")
		 CountPallet = JSON.GetChildCount(ArrayPallet)
		 
		 For FilaP = 1 To CountPallet
			 IndexPallet = JSON.GetChildItem(ArrayPallet, FilaP)
			 New	=	dw_5.InsertRow(0)
			  
			Pallet = JSON.GetItemString(IndexPallet, "p_pallet")
			dw_5.SetItem(New, "p_pallet", Pallet)
			
			Atributo = JSON.GetItemString(IndexPallet, "p_camara")
			dw_5.SetItem(New, "p_camara", Atributo)
			
			Atributo = JSON.GetItemString(IndexPallet, "p_nivel")
			dw_5.SetItem(New, "p_nivel", Atributo)
			Atributo = JSON.GetItemString(IndexPallet, "p_contenedor")
			dw_5.SetItem(New, "p_contenedor", Atributo)
			
			Atributo = JSON.GetItemString(IndexPallet, "p_termografo")
			dw_5.SetItem(New, "p_termografo", Atributo)
			
			Atributo = JSON.GetItemString(IndexPallet, "p_temperatura")
			dw_5.SetItem(New, "p_temperatura", Atributo)
			
			Atributo = JSON.GetItemString(IndexPallet, "p_observaciones")
			dw_5.SetItem(New, "p_observaciones", Atributo)
			
			ArrayDetalle = JSON.GetItemArray(IndexPallet, "p_detalle")
			CountDetalle= JSON.GetChildCount(ArrayDetalle)
			
			For FilaD = 1 To CountDetalle
				IndexDetalle	= JSON.GetChildItem(ArrayDetalle, FilaD)
				New	=	dw_6.InsertRow(0)
				
				dw_6.SetItem(New, "p_pallet", Pallet)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_especie")
				dw_6.SetItem(New, "p_especie", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_variedad")
				dw_6.SetItem(New, "p_variedad", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_atributo")
				dw_6.SetItem(New, "p_atributo", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_cajas")
				dw_6.SetItem(New, "p_cajas", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_etiqueta")
				dw_6.SetItem(New, "p_etiqueta", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_calidad")
				dw_6.SetItem(New, "p_calidad", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_calibre")
				dw_6.SetItem(New, "p_calibre", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_fecha")
				dw_6.SetItem(New, "p_fecha", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_productor")
				dw_6.SetItem(New, "p_productor", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_codembalaje")
				dw_6.SetItem(New, "p_codembalaje", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_embalaje")
				dw_6.SetItem(New, "p_embalaje", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_kn")
				dw_6.SetItem(New, "p_kn", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_kb")
				dw_6.SetItem(New, "p_kb", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_embalajejet")
				dw_6.SetItem(New, "p_embalajejet", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_guia")
				dw_6.SetItem(New, "p_guia", Atributo)
				
				Atributo = JSON.GetItemString(IndexDetalle, "p_packing")
				dw_6.SetItem(New, "p_packing", Atributo)
			Next
		Next
	Next
Else
	Return -1
End if

wf_actualiza_db()

Return 0

end event

type dw_3 from uo_dw within w_cargaembarque
boolean visible = false
integer x = 59
integer y = 1340
integer width = 731
integer height = 372
integer taborder = 31
boolean bringtotop = true
string dataobject = "dw_viaje"
boolean vscrollbar = false
end type

event sqlpreview;//
end event

type dw_4 from uo_dw within w_cargaembarque
boolean visible = false
integer x = 814
integer y = 1340
integer width = 731
integer height = 372
integer taborder = 41
boolean bringtotop = true
string dataobject = "dw_envio"
boolean vscrollbar = false
end type

event sqlpreview;//
end event

type dw_5 from uo_dw within w_cargaembarque
boolean visible = false
integer x = 1568
integer y = 1340
integer width = 731
integer height = 372
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_pallet"
boolean vscrollbar = false
end type

event sqlpreview;//
end event

type dw_6 from uo_dw within w_cargaembarque
boolean visible = false
integer x = 2322
integer y = 1340
integer width = 731
integer height = 372
integer taborder = 21
boolean bringtotop = true
string dataobject = "dw_pallet_detalle"
boolean vscrollbar = false
end type

event sqlpreview;//
end event

